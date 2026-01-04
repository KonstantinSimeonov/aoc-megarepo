use std::env;
use std::fs;

#[derive(Debug)]
pub struct Inputs {
    pub input: String,
    pub test_inputs: Vec<String>,
}

impl Inputs {
    pub fn read() -> Inputs {
        let exe = env::current_exe().unwrap();
        let name = exe.file_name().unwrap().to_str().unwrap();

        let dir_path = format!("./src/bin/{}", name);
        let input_names = fs::read_dir(dir_path).unwrap().map(|x| x.unwrap().path());

        let mut result = Inputs {
            input: "".to_string(),
            test_inputs: vec![],
        };

        for entry in input_names {
            let path = entry.to_str().unwrap();
            let content = fs::read_to_string(path).unwrap().trim().to_string();

            if entry.extension().is_some() {
                continue;
            }

            if path.ends_with("input") {
                result.input = content;
            } else {
                result.test_inputs.push(content);
            }
        }

        result
    }
}
