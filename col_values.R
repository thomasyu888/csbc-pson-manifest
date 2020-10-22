# columns required of manifests
template <- list(
  "publication" = c(
    "doi", "journal", "pubMedId", "bioProjectIds", "bioProjectAccns",
    "pubMedUrl", "publicationTitle", "publicationYear", "keywords", "mesh",
    "authors", "consortium", "grantId", "grantNumber", "gseAccns", "gseUrls",
    "srxAccns", "srxUrls", "srpAccns", "srpUrls", "dbgapAccns", "dbgapUrls",
    "dataset", "tool", "assay", "tumorType", "tissue"
  ),

  "dataset" = c(
    "datasetId", "datasetUrl", "datasetName", "description", "overallDesign",
    "assay", "species", "tumorType", "tissue", "grantNumber", "publicationTitle",
    "externalLinkID", "externalLink", "synapseID"
  ),  

  "file" = c(
    "fileName", "fileUrl", "datasetName", "datasetId", "title", "assay",
    "platform", "dataFormat", "species", "sex", "tumorType", "tissue",
    "grantNumber"
  ),  

  "tool" = c(
    "tool", "description", "toolType", "softwareLanguage", "inputDataType",
    "outputDataType", "grantNumber", "publicationTitle", "externalLink"
  )
)

# column in manifest that needs to have unique IDs/names
id <- list(
  "publication" = "pubMedId",
  "dataset" = "datasetId",
  "file" = "fileName",
  "tool" = "tool"
)

# columns that require annotations (no NAs or empty strings)
complete_cols <- list(
  "publication" = c(
    "pubMedId", "publicationTitle", "grantNumber", "consortium"
  ),
  "dataset" = c("datasetId", "datasetName", "publicationTitle"),
  "file" = c("fileName", "datasetName"),
  "tool" = c("tool")
)

# columns that should have standardized values
std_cols <- c(
  "dataFormat", "dataType", "tumorType", "tissue", "platform", "assay", "sex",
  "species", "toolType", "softwareLanguage", "inputDataType", "outputDataType"
)

