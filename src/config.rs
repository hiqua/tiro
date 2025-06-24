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
use anyhow::Result as TiroResult;

pub type Category = str;

/// The global config.
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

/// This is needed as the intermediate object to deserialize because the Toml
/// crate cannot parse the enum Quadrant directly in this case, as it's part of
/// a map.
#[derive(Debug, Serialize, Deserialize)]
struct RawConfig {
    activity_paths: Vec<PathBuf>,
    quadrants: HashMap<String, Vec<String>>,
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeSet, HashMap};
    use std::env;
    use std::fs::{self, File};
    use std::io::Write;
    use std::path::PathBuf;

    use crate::config::{load_config, update_parse_state_from_config, Config, Quadrant};
    use crate::parse_state::ParseState;
    use anyhow::Error as TiroError; // For asserting error types - now anyhow::Error

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
    struct TestLocalConfig {
        // Renamed from Config to avoid conflict
        activity_paths: Vec<PathBuf>,
        quadrants: HashMap<String, Vec<String>>,
    }

    #[test]
    fn test_2() {
        let config: TestLocalConfig = toml::from_str(
            // Use TestLocalConfig
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

    // Helper function to create a temporary directory for test files
    fn setup_test_dir(suffix: &str) -> PathBuf {
        let base_dir = PathBuf::from("./target/test_temp_configs");
        // fs::create_dir_all(&base_dir).expect("Failed to create base test_temp_configs dir"); // Ensure base exists
        let test_specific_dir = base_dir.join(suffix);
        fs::create_dir_all(&test_specific_dir).expect(&format!(
            "Failed to create test specific temp dir: {:?}",
            test_specific_dir
        ));
        test_specific_dir
    }

    // Helper function to clean up the temporary directory
    fn cleanup_test_dir(test_dir: &PathBuf) {
        if test_dir.exists() {
            fs::remove_dir_all(test_dir).expect("Failed to remove test temp dir");
        }
    }

    #[test]
    fn test_load_config_valid_file() {
        let test_dir = setup_test_dir("valid_file");
        let activity_file_path_relative = test_dir.join("activities.txt");
        let config_file_path = test_dir.join("valid_config.toml");

        // Create dummy activity file
        File::create(&activity_file_path_relative)
            .expect("Failed to create dummy activity file")
            .write_all(b"dummy content")
            .expect("Failed to write to dummy activity file");

        // Get the absolute path to write into the config file
        let activity_file_path_abs = fs::canonicalize(&activity_file_path_relative)
            .expect("Failed to canonicalize activity_file_path for config content");

        // Create valid config content
        let config_content = format!(
            r#"
            activity_paths = ["{}"]

            [quadrants]
            Q1 = ["@work"]
            Q2 = ["@home", "@relax"]
            "#,
            activity_file_path_abs.to_str().unwrap() // Ensure absolute path string in TOML
        );
        fs::write(&config_file_path, config_content).expect("Failed to write valid config file");

        let result = load_config(config_file_path.to_str().unwrap());
        assert!(
            result.is_ok(),
            "load_config failed for a valid file: {:?}",
            result.err()
        );

        let config = result.unwrap();

        // Assert quadrants
        assert_eq!(config.quadrants.len(), 2);
        assert_eq!(
            config.quadrants.get(&Quadrant::Q1),
            Some(&vec!["@work".to_string()])
        );
        assert_eq!(
            config.quadrants.get(&Quadrant::Q2),
            Some(&vec!["@home".to_string(), "@relax".to_string()])
        );

        // Assert activity_paths
        let expected_activity_path = activity_file_path_abs; // Already canonicalized
        assert_eq!(config.activity_paths.len(), 1);
        assert!(config.activity_paths.contains(&expected_activity_path));

        cleanup_test_dir(&test_dir);
    }

    #[test]
    fn test_load_config_invalid_toml_syntax() {
        let test_dir = setup_test_dir("invalid_syntax");
        let config_file_path = test_dir.join("invalid_syntax.toml");

        let invalid_config_content = r#"
            activity_paths = ["/some/path"]
            [quadrants]
            Q1 = ["@test"]
            THIS_IS_INVALID_TOML_SYNTAX
        "#;
        fs::write(&config_file_path, invalid_config_content)
            .expect("Failed to write invalid config file");

        let result = load_config(config_file_path.to_str().unwrap());
        assert!(
            result.is_err(),
            "load_config did not return an error for invalid TOML syntax"
        );

        // Check if the error is a TOML parsing error
        if let Err(err) = result {
            assert!(err.downcast_ref::<toml::de::Error>().is_some() || err.to_string().contains("TOML"), "Error message does not indicate a TOML parsing error: {}", err);
        } else {
            panic!("Expected an error containing a TOML parsing error");
        }

        cleanup_test_dir(&test_dir);
    }

    #[test]
    fn test_load_config_non_existent_file() {
        let test_dir = setup_test_dir("non_existent");
        let non_existent_config_path = test_dir.join("non_existent_config.toml");

        // Ensure file does not exist before test
        if non_existent_config_path.exists() {
            fs::remove_file(&non_existent_config_path).unwrap();
        }

        let result = load_config(non_existent_config_path.to_str().unwrap());
        assert!(
            result.is_err(),
            "load_config did not return an error for a non-existent file"
        );

        // Check if the error is an IO error (file not found)
        if let Err(err) = result {
            assert!(err.downcast_ref::<std::io::Error>().is_some() || err.to_string().contains("No such file"), "Error message does not indicate file not found: {}", err);
        } else {
            panic!("Expected an error indicating file not found");
        }
        cleanup_test_dir(&test_dir);
    }

    #[test]
    fn test_update_parse_state_basic() {
        let mut config = Config::default();
        config.quadrants.insert(
            Quadrant::Q1,
            vec!["@work".to_string(), "@urgent".to_string()],
        );
        config
            .quadrants
            .insert(Quadrant::Q2, vec!["@home".to_string()]);

        let mut parse_state = ParseState::new();

        update_parse_state_from_config(&config, &mut parse_state).unwrap();

        assert_eq!(parse_state.categories_to_quadrant.len(), 3);
        assert_eq!(
            parse_state.categories_to_quadrant.get("@work"),
            Some(&Quadrant::Q1)
        );
        assert_eq!(
            parse_state.categories_to_quadrant.get("@urgent"),
            Some(&Quadrant::Q1)
        );
        assert_eq!(
            parse_state.categories_to_quadrant.get("@home"),
            Some(&Quadrant::Q2)
        );
    }

    #[test]
    fn test_update_parse_state_no_override() {
        let mut config = Config::default();
        config
            .quadrants
            .insert(Quadrant::Q1, vec!["@work".to_string()]); // Config tries to set @work to Q1

        let mut parse_state = ParseState::new();
        parse_state
            .categories_to_quadrant
            .insert("@work".to_string(), Quadrant::Q2); // ParseState already has @work as Q2
        parse_state
            .categories_to_quadrant
            .insert("@study".to_string(), Quadrant::Q3);

        update_parse_state_from_config(&config, &mut parse_state).unwrap();

        assert_eq!(parse_state.categories_to_quadrant.len(), 2);
        assert_eq!(
            parse_state.categories_to_quadrant.get("@work"),
            Some(&Quadrant::Q2),
            "Existing mapping for @work should not be overridden"
        );
        assert_eq!(
            parse_state.categories_to_quadrant.get("@study"),
            Some(&Quadrant::Q3)
        );
    }

    #[test]
    fn test_update_parse_state_category_not_in_config() {
        let mut config = Config::default();
        config
            .quadrants
            .insert(Quadrant::Q1, vec!["@work".to_string()]);

        let mut parse_state = ParseState::new();
        parse_state
            .categories_to_quadrant
            .insert("@personal".to_string(), Quadrant::Q4); // This category is not in config

        update_parse_state_from_config(&config, &mut parse_state).unwrap();

        assert_eq!(parse_state.categories_to_quadrant.len(), 2);
        assert_eq!(
            parse_state.categories_to_quadrant.get("@work"),
            Some(&Quadrant::Q1),
            "@work from config should be added"
        );
        assert_eq!(
            parse_state.categories_to_quadrant.get("@personal"),
            Some(&Quadrant::Q4),
            "@personal should remain unchanged"
        );
    }

    #[test]
    fn test_update_parse_state_empty_parse_state() {
        let mut config = Config::default();
        config
            .quadrants
            .insert(Quadrant::Q1, vec!["@test".to_string()]);

        let mut parse_state = ParseState::new(); // Empty ParseState

        update_parse_state_from_config(&config, &mut parse_state).unwrap();

        assert_eq!(parse_state.categories_to_quadrant.len(), 1);
        assert_eq!(
            parse_state.categories_to_quadrant.get("@test"),
            Some(&Quadrant::Q1)
        );
    }

    #[test]
    fn test_update_parse_state_empty_config_quadrants() {
        let config = Config::default(); // Empty Config.quadrants

        let mut parse_state = ParseState::new();
        parse_state
            .categories_to_quadrant
            .insert("@existing".to_string(), Quadrant::Q1);

        let initial_parse_state_clone = parse_state.categories_to_quadrant.clone();

        update_parse_state_from_config(&config, &mut parse_state).unwrap();

        assert_eq!(
            parse_state.categories_to_quadrant, initial_parse_state_clone,
            "ParseState should remain unchanged with empty config quadrants"
        );
        assert_eq!(parse_state.categories_to_quadrant.len(), 1);
        assert_eq!(
            parse_state.categories_to_quadrant.get("@existing"),
            Some(&Quadrant::Q1)
        );
    }
}

pub enum MetaCategory<'a> {
    RegularCategory {
        description: &'a str,
        // global_quad: Option<Quadrant>, // Unused
    },
    Quad {
        quadrant: Quadrant,
    },
}

#[derive(Copy, Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Quadrant {
    Q1,
    Q2,
    Q3,
    Q4,
    Q5,
    #[default]
    Q6,
}

impl FromStr for Quadrant {
    type Err = anyhow::Error;

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
            _ => Err(anyhow::anyhow!("could not parse quadrant: {}", s)),
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
        .filter(|f| {
            if f.exists() {
                true
            } else {
                eprintln!("Filtered out non-existent activity file: {:?}", f);
                false
            }
        })
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
