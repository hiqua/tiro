use core::fmt;
use std::collections::{BTreeSet, HashMap};
use std::env;
use std::fs;
use std::fs::canonicalize;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use clap::ArgMatches;
use colored::*;
use serde_derive::Deserialize;
use serde_derive::Serialize;
use toml::de::Error;

use crate::parse_state::ParseState;
use crate::{TiroError, TiroResult};

pub type Category = str;

impl From<std::io::Error> for TiroError {
    fn from(e: std::io::Error) -> Self {
        TiroError { e: e.to_string() }
    }
}

impl From<Error> for TiroError {
    fn from(e: toml::de::Error) -> Self {
        TiroError { e: e.to_string() }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct Config {
    pub(crate) quadrants: HashMap<Quadrant, Vec<String>>,
    pub notify: bool,
    pub quiet: bool,
    pub watch: bool,
    pub summary_out: Option<String>,
    pub plan_out: Option<String>,
    activity_paths: BTreeSet<PathBuf>,
}

impl Config {
    fn add_activity_path(&mut self, p: PathBuf) {
        self.activity_paths.insert(canonicalize(p).unwrap());
    }

    pub fn get_file_paths(&self) -> BTreeSet<PathBuf> {
        self.activity_paths.clone()
    }
}

/// This is needed because the toml crate cannot parse enum directly.
#[derive(Debug, Serialize, Deserialize)]
struct RawConfig {
    activity_paths: Vec<PathBuf>,
    quadrants: HashMap<String, Vec<String>>,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::env;
    use std::path::PathBuf;

    use serde_derive::Deserialize;
    use serde_derive::Serialize;

    #[derive(Debug, Serialize, Deserialize)]
    struct Pb {
        p: PathBuf,
    }

    #[test]
    fn parse_as_path_buf() {
        let config: Pb = toml::from_str(
            r#"
            p = "/$MOBILE_DIR/0_planning/activities.txt"
                "#,
        )
        .unwrap();

        println!("{:?}", config);
        let mut new_p = PathBuf::new();
        for part in config.p.iter() {
            println!("{:?}", part);
            if part.to_str().unwrap().starts_with('$') {
                let mut var = part.to_str().unwrap().to_string();
                var.drain(..1);
                println!("var: {}", var);
                if let Ok(r) = env::var(var) {
                    println!("found!");
                    new_p.push(r);
                    continue;
                }
            }
            new_p.push(part);
        }

        println!("Res: {:?}", new_p);
    }

    #[derive(Serialize, Deserialize)]
    struct Config {
        activity_paths: Vec<PathBuf>,
        quadrants: HashMap<String, Vec<String>>,
    }

    #[test]
    fn test_2() {
        let config: Config = toml::from_str(
            r#"activity_paths = ["$MOBILE_DIR/0_planning/activities.txt", "other_path"]
                [quadrants]
                # urgent and important
                "Q1" = []

                # non-urgent and important
                "Q2" = [
                  "@sleep",
                  "@music",
                ]
                "#,
        )
        .unwrap();
        println!("{:?}", config.activity_paths);
        println!("{:?}", config.quadrants);
    }
}

pub enum MetaCategory<'a> {
    RegularCategory {
        description: &'a str,
        global_quad: Option<Quadrant>,
    },
    Quad {
        quadrant: Quadrant,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Quadrant {
    Q1,
    Q2,
    Q3,
    Q4,
    Q5,
    Q6,
}

impl Default for Quadrant {
    fn default() -> Self {
        Quadrant::Q6
    }
}

impl FromStr for Quadrant {
    type Err = TiroError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "@1" => Ok(Quadrant::Q1),
            "@2" => Ok(Quadrant::Q2),
            "@3" => Ok(Quadrant::Q3),
            "@4" => Ok(Quadrant::Q4),
            "@5" => Ok(Quadrant::Q5),
            "@6" => Ok(Quadrant::Q6),
            "1" => Ok(Quadrant::Q1),
            "2" => Ok(Quadrant::Q2),
            "3" => Ok(Quadrant::Q3),
            "4" => Ok(Quadrant::Q4),
            "5" => Ok(Quadrant::Q5),
            "6" => Ok(Quadrant::Q6),
            "Q1" => Ok(Quadrant::Q1),
            "Q2" => Ok(Quadrant::Q2),
            "Q3" => Ok(Quadrant::Q3),
            "Q4" => Ok(Quadrant::Q4),
            "Q5" => Ok(Quadrant::Q5),
            "Q6" => Ok(Quadrant::Q6),
            _ => Err(TiroError {
                e: "could not parse quadrant".to_string(),
            }),
        }
    }
}

impl fmt::Display for Quadrant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Quadrant::Q1 => write!(f, "{}", "@1".red().bold()),
            Quadrant::Q2 => write!(f, "{}", "@2".yellow().bold()),
            Quadrant::Q3 => write!(f, "{}", "@3".cyan().bold()),
            Quadrant::Q4 => write!(f, "{}", "@4".white().bold()),
            Quadrant::Q5 => write!(f, "{}", "@5".purple()),
            Quadrant::Q6 => write!(f, "{}", "@6".normal()),
        }
    }
}

fn convert_raw_config(raw_config: RawConfig) -> TiroResult<Config> {
    let mut map = HashMap::new();

    for (k, v) in raw_config.quadrants.iter() {
        map.insert(Quadrant::from_str(k)?, v.clone());
    }

    let activity_paths = raw_config
        .activity_paths
        .iter()
        .map(|p| p.as_path())
        .map(substitute_env_variable)
        .map(|f| {
            f.canonicalize()
                .expect("Could not canonicalize an activity path.")
        })
        .collect();

    Ok(Config {
        activity_paths,
        quadrants: map,
        ..Default::default()
    })
}

pub fn load_config_from_matches(matches: &ArgMatches) -> Config {
    let config_paths = matches.values_of("config").expect("");
    let mut conf = Config::default();

    for config_path in config_paths {
        conf = load_config(config_path).expect("Cannot proceed without valid configuration path.")
    }

    conf.notify |= matches.is_present("notify");
    conf.quiet |= matches.is_present("quiet");
    conf.watch |= matches.is_present("watch");

    for p in get_activity_file_path_from_matches(matches) {
        conf.add_activity_path(p);
    }

    if conf.summary_out.is_none() {
        conf.summary_out = matches.value_of("summary").map(|s| s.to_string());
    }
    conf.plan_out = matches.value_of("plan").map(|s| s.to_string());
    conf
}

fn get_activity_file_path_from_matches(matches: &ArgMatches) -> Vec<PathBuf> {
    let mut res = vec![];
    if let Some(paths) = matches.values_of("activities") {
        for path in paths {
            res.push(PathBuf::from_str(path).unwrap());
        }
    }
    res
}

fn load_config(path: &str) -> TiroResult<Config> {
    let config_str = fs::read_to_string(path)?;
    let raw_config: RawConfig = toml::from_str(&config_str)?;

    convert_raw_config(raw_config)
}

/// XXX
/// * don't use side-effects
/// * handle errors properly
pub fn update_parse_state_from_config(
    config: &Config,
    parse_state: &mut ParseState,
) -> TiroResult<()> {
    for (q, v) in config.quadrants.iter() {
        for s in v {
            if !parse_state.categories_to_quadrant.contains_key(s) {
                parse_state.categories_to_quadrant.insert(s.to_string(), *q);
            }
        }
    }

    Ok(())
}

/// Replace $VAR in path, when the full component is such a var.
/// I could use a crate for that, but it's simpler to just have a small function for now.
fn substitute_env_variable(path: &Path) -> PathBuf {
    let mut new_path = PathBuf::new();
    for part in path.iter() {
        if part.to_str().unwrap().starts_with('$') {
            let mut var = part.to_str().unwrap().to_string();
            var.drain(..1);
            if let Ok(r) = env::var(var) {
                new_path.push(r);
                continue;
            }
        }
        new_path.push(part);
    }

    new_path
}
