use std::fs;
use tempfile::NamedTempFile;

pub struct PharmacyFixture {
    file: NamedTempFile,
    specs: Vec<SpecFixture>,
}

impl PharmacyFixture {
    pub fn new() -> Self {
        PharmacyFixture {
            file: NamedTempFile::new().unwrap(),
            specs: Vec::new(),
        }
    }

    pub fn get_path(&self) -> &str {
        self.file.path().to_str().unwrap()
    }

    pub fn add_matched_spec(&mut self) {
        let rx = ValidPrescriptionFixture::new();
        let doc = rx.make_document_match();
        self.add_spec(SpecFixture {
            prescription: Box::new(rx),
            document: Box::new(doc),
        });
    }

    pub fn add_mismatched_spec(&mut self) {
        let rx = ValidPrescriptionFixture::new();
        let doc = rx.make_document_mismatch();
        self.add_spec(SpecFixture {
            prescription: Box::new(rx),
            document: Box::new(doc),
        });
    }

    pub fn add_missing_doc_spec(&mut self) {
        let rx = ValidPrescriptionFixture::new();
        let doc = MissingDocumentFixture::new();
        self.add_spec(SpecFixture {
            prescription: Box::new(rx),
            document: Box::new(doc),
        });
    }

    pub fn add_missing_prescription_spec(&mut self) {
        let rx = MissingPrescriptionFixture::new();
        let doc = DocumentFixture {
            content: String::new(),
            file: NamedTempFile::new().unwrap(),
        };
        self.add_spec(SpecFixture {
            prescription: Box::new(rx),
            document: Box::new(doc),
        });
    }

    fn add_spec(&mut self, spec: SpecFixture) {
        spec.persist();
        self.specs.push(spec);
        self.persist();
    }

    fn persist(&mut self) {
        fs::write(self.file.path(), self.to_string().as_bytes()).unwrap();
    }

    fn to_string(&self) -> String {
        let mut pharmacy = "[Specs]\n".to_string();
        for spec in &self.specs {
            pharmacy += format!("{}\n", spec.to_string()).as_str();
        }

        pharmacy
    }
}

struct SpecFixture {
    prescription: Box<Document>,
    document: Box<Document>,
}

impl SpecFixture {
    fn to_string(&self) -> String {
        format!(
            "\"{}\" = \"{}\"",
            self.prescription.get_path(),
            self.document.get_path()
        )
    }

    fn persist(&self) {
        self.prescription.persist();
        self.document.persist();
    }
}

trait Document {
    fn get_content(&self) -> String;

    fn get_path(&self) -> String;

    fn persist(&self);
}

struct ValidPrescriptionFixture {
    content: String,
    file: NamedTempFile,
}

impl Document for ValidPrescriptionFixture {
    fn get_content(&self) -> String {
        self.content.clone()
    }

    fn get_path(&self) -> String {
        String::from(self.file.path().to_str().unwrap())
    }

    fn persist(&self) {
        fs::write(self.get_path(), self.get_content()).unwrap();
    }
}

struct MissingPrescriptionFixture {
    path: String,
}

impl MissingPrescriptionFixture {
    fn new() -> Self {
        let rx = NamedTempFile::new().unwrap();
        MissingPrescriptionFixture {
            path: rx.path().to_str().unwrap().to_string(),
        }
    }
}

impl Document for MissingPrescriptionFixture {
    fn get_content(&self) -> String {
        String::new()
    }

    fn get_path(&self) -> String {
        self.path.clone()
    }

    fn persist(&self) {
        ()
    }
}

struct DocumentFixture {
    content: String,
    file: NamedTempFile,
}

impl Document for DocumentFixture {
    fn get_content(&self) -> String {
        self.content.clone()
    }

    fn get_path(&self) -> String {
        String::from(self.file.path().to_str().unwrap())
    }

    fn persist(&self) {
        fs::write(self.get_path(), self.get_content()).unwrap();
    }
}

pub struct MissingDocumentFixture {
    path: String,
}

impl MissingDocumentFixture {
    fn new() -> Self {
        let doc = NamedTempFile::new().unwrap();
        MissingDocumentFixture {
            path: doc.path().to_str().unwrap().to_string(),
        }
    }
}

impl Document for MissingDocumentFixture {
    fn get_content(&self) -> String {
        String::new()
    }

    fn get_path(&self) -> String {
        self.path.clone()
    }

    fn persist(&self) {
        ()
    }
}

impl ValidPrescriptionFixture {
    pub fn new() -> Self {
        ValidPrescriptionFixture {
            content: "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed a augue vel \
                      ipsum aliquam porttitor. Curabitur feugiat, diam eget tincidunt. "
                .to_string(),
            file: NamedTempFile::new().unwrap(),
        }
    }

    pub fn make_document_match(&self) -> DocumentFixture {
        DocumentFixture {
            content: self.content.clone(),
            file: NamedTempFile::new().unwrap(),
        }
    }

    pub fn make_document_mismatch(&self) -> DocumentFixture {
        let mut content = self.content.clone();
        content.truncate(self.content.len() - 5);
        DocumentFixture {
            content,
            file: NamedTempFile::new().unwrap(),
        }
    }
}
