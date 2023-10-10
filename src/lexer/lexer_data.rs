pub struct LexerData {
    user_data_types: Vec<String>,
    generic_data_types: Vec<String>,
}

impl LexerData {
    pub fn new() -> Self {
        LexerData {
            user_data_types: Vec::new(),
            generic_data_types: Vec::new(),
        }
    }

    pub fn add_user_data_type(&mut self, data_type: String) {
        self.user_data_types.push(data_type);
    }

    pub fn add_generic_data_type(&mut self, data_type: String) {
        self.generic_data_types.push(data_type);
    }

    pub fn get_user_data_types(&self) -> &Vec<String> {
        &self.user_data_types
    }

    pub fn get_generic_data_types(&self) -> &Vec<String> {
        &self.generic_data_types
    }

    pub fn clear_generic_data_types(&mut self) {
        self.generic_data_types.clear();
    }
}
