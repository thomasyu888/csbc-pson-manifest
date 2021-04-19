## Prettify a string that will cater to Synapse naming rules, e.g.
## can only contain letters, numbers, spaces, underscores, hypens,
## periods, plus signs, apostrophes, and parentheses.
syn_prettify <- function(name) {
  stringr::str_replace_all(name, c(":" = "-", ";" = "-", "/" = "_"))
}


## Create a "list" of annotations; some of the table schemas require
## a column to be a StringList, e.g. '["a", "b"]' instead of 'a, b'.
create_listed_annots <- function(annots, delim = ",") {
  split_annots <- lapply(stringr::str_split(annots, delim), trimws)[[1]]
  res <- paste0(
    "[\"",
    paste0(split_annots, collapse = "\", \""),
    "\"]"
  )
  res
}


## Create markdown for URL
create_url_markdown <- function(link_text, url, param = "") {
    paste0("[", link_text, "](", url, param, ")")
}


## Split annots and search for match in table
split_and_search <- function(annots, table, search_col, output_col,
                             remove_chars = TRUE) {
  res <- lapply(stringr::str_split(annots, ", ")[[1]], function(annot) {
    stringr::str_split(
      table[grepl(paste0("^", annot, "$"), table[[search_col]], ignore.case = TRUE), ][[output_col]], #nolint
      ", "
    )
  })

  # get rid of NAs and unnecessary characters
  res <- unlist(res[!is.na(res)])
  if (isTRUE(remove_chars)) {
    res <- stringr::str_replace_all(res, c("\\[" = "", "\\\"" = "", "\\]" = ""))
  }
  paste0(unique(res), collapse = ", ")
}


## Save new entity to Synapse; Folder for dataset manifest, File for others
save_file_to_synapse <- function(syn, synapseclient, name,
                                 parent, annotations) {
  name <- syn_prettify(name)

  # create dummy file to upload to Synapse
  write(name, file = name)
  new_file <- synapseclient$File(
    path = name,
    name = name,
    parent = parent,
    annotations = annotations
  )
  new_file <- syn$store(new_file)

  # remove dummy file
  file.remove(name)

  new_file$id
}
save_folder_to_synapse <- function(syn, synapseclient, name,
                                   parent, annotations) {
  name <- syn_prettify(name)
  new_folder <- synapseclient$Folder(
    name,
    parent = parent,
    annotations = annotations
  )
  new_folder <- syn$store(new_folder)
  new_folder$id
}

## Set annotations for a given entity, depending on manifest type.
publication_annots <- function(manifest) {
  list(
    doi = manifest[["doi"]],
    title = manifest[["publicationTitle"]],
    journal = manifest[["journal"]],
    publicationYear = manifest[["publicationYear"]],
    keywords = manifest[["keywords"]],
    pubMedUrl = manifest[["pubMedUrl"]],
    pubMedLink = manifest[["pubMedUrl"]],
    assay = manifest[["assay"]],
    tissue = manifest[["tissue"]],
    tumorType = manifest[["tumorType"]]
  )
}
dataset_annots <- function(manifest) {
  list(
    fullName = manifest[["datasetName"]],
    displayName = manifest[["datasetId"]]
  )
}
tool_annots <- function(manifest, grants) {
  # some tools may be annotated with multiple grants.
  split_grants <- lapply(
    stringr::str_split(manifest[["grantNumber"]], ","), trimws
  )[[1]]
  list(
    displayName = manifest[["tool"]],
    grantId = paste(grants[grants$grantNumber == split_grants, ]$grantId, collapse = ", "), #nolint
    toolType = manifest[["toolType"]]
  )
}
file_annots <- function(manifest, parent, grants, datasets) {
  list(
    fileName = manifest[["fileName"]],
    name = manifest[["fileName"]],
    datasets = split_and_search(manifest[["datasetName"]], datasets, "datasetName", "datasetAlias"), #nolint
    parentId = parent,
    assay = manifest[["assay"]],
    platform = manifest[["platform"]],
    dataFormat = manifest[["dataFormat"]],
    species = manifest[["species"]],
    gender = manifest[["sex"]],
    tumorType = manifest[["tumorType"]],
    tissue = manifest[["tissue"]],
    grantName = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "grantName"), #nolint
    grantType = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "grantType"), #nolint
    consortium = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "consortium") #nolint
  )
}

## Create tibble for entry into * - Merged tables.
publication_row <- function(syn_id, manifest, grants, datasets) {
  tibble(
    publicationId = syn_id,
    doi = manifest[["doi"]],
    journal = manifest[["journal"]],
    pubMedId = manifest[["pubMedId"]],
    pubMedUrl = manifest[["pubMedUrl"]],
    pubMedLink = create_url_markdown(paste0("PMID:", manifest[["pubMedId"]]), manifest[["pubMedUrl"]]), #nolint
    publicationTitle = manifest[["publicationTitle"]],
    publicationYear = manifest[["publicationYear"]],
    keywords = manifest[["keywords"]],
    authors = manifest[["authors"]],
    assay = create_listed_annots(manifest[["assay"]]),
    tumorType = create_listed_annots(manifest[["tumorType"]]),
    tissue = create_listed_annots(manifest[["tissue"]], delim =";"),
    themeId = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "themeId"), #nolint
    theme = create_listed_annots(split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "theme")), #nolint
    consortiumId = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "consortiumId"), #nolint
    consortium = create_listed_annots(manifest[["consortium"]]),
    grantId = create_listed_annots(manifest[["grantId"]]),
    grantName = create_listed_annots(split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "grantName")), #nolint
    grantNumber = create_listed_annots(manifest[["grantNumber"]]),
    grantInstitution = create_listed_annots(split_and_search(
      manifest[["grantNumber"]], grants, 
      "grantNumber", "grantInstitution"
    )),
    datasetId = ifelse(
      manifest[["bioProjectAccns"]] %in% datasets$datasetAlias,
      split_and_search(manifest[["bioProjectAccns"]], datasets, "datasetAlias", "datasetId"), # nolint
      ""
    ),
    dataset = manifest[["dataset"]]
  )
}

#' Get Synapse tables used for the portal.
get_tables <- function(syn) {
  grants <- get_portal_table(
    syn,
    portal_table[["grant"]],
    c("grantId", "grantName", "grantNumber", "grantInstitution",
      "themeId", "theme", "consortiumId", "consortium")
  )
  publications <- get_portal_table(
    syn,
    portal_table[["publication"]],
    c("publicationId", "publicationTitle", "grantId", "grantNumber",
      "grantName", "themeId", "theme", "consortiumId", "consortium"
    )
  )
  datasets <- get_portal_table(
    syn,
    portal_table[["dataset"]],
    c("datasetId", "datasetName", "datasetAlias")
  )
  tools <- get_portal_table(
    syn,
    portal_table[["tool"]],
    c("toolId", "toolName")
  )
  files <- get_portal_table(
    syn,
    portal_table[["file"]],
    c("fileName", "datasets", "parentId")
  )
  list(
    "grants" = grants,
    "publications" = publications,
    "datasets" = datasets,
    "files" = files,
    "tools" = tools
  )
}

#' Get selected coloumns from a Synapse table.
get_portal_table <- function(syn, table_id, cols) {
  query <- sprintf("SELECT %s FROM %s", paste0(cols, collapse=","), table_id)
  return(syn$tableQuery(query)$asDataFrame())
}


dataset_row <- function(syn_id, manifest, publications) {
  tibble(
    datasetId = syn_id,
    datasetName = manifest[["datasetName"]],
    datasetAlias = manifest[["datasetId"]],
    description = manifest[["description"]],
    overallDesign = manifest[["overallDesign"]],
    assay = create_listed_annots(manifest[["assay"]]),
    species = create_listed_annots(manifest[["species"]]),
    tumorType = create_listed_annots(manifest[["tumorType"]]),
    themeId = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "themeId"), #nolint
    theme = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "theme")), #nolint
    consortiumId = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "consortiumId"), #nolint
    consortium = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "consortium")), #nolint
    grantId = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantId")), #nolint
    grantName = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantName")), #nolint
    grantNumber = ifelse(
        manifest[["grantNumber"]] == "",
        create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantNumber")), #nolint
        create_listed_annots(manifest[["grantNumber"]])
    ),
    #publicationId = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "publicationId")), #nolint
    publicationId = NA,
    publicationTitle = create_listed_annots(manifest[["publicationTitle"]]),
    publication = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "pubMedLink", remove_chars = FALSE)), #nolint
    externalLink = ifelse(
      manifest[["externalLink"]] == "",
      create_url_markdown(
        paste0(
          if (startsWith(manifest[["datasetId"]], "GSE")) {
            "GEO:"
          } else if (startsWith(manifest[["datasetId"]], "SRP")) {
            "SRA:"
          } else {
            ""
          },
          manifest[["datasetId"]]
        ),
        manifest[["datasetUrl"]]
      ),
      manifest[["externalLink"]]
    )    
  )
}
tool_row <- function(syn_id, manifest, publications) {
  tibble(
    toolId = syn_id,
    toolName = manifest[["tool"]],
    description = manifest[["description"]],
    homepageUrl = manifest[["externalLink"]],
    toolType = manifest[["toolType"]],
    softwareLanguage = create_listed_annots(manifest[["softwareLanguage"]]),
    inputDataType = create_listed_annots(manifest[["inputDataType"]]),
    outputDataType = create_listed_annots(manifest[["outputDataType"]]),
    themeId = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "themeId"), #nolint
    theme = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "theme")), #nolint
    consortiumId = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "consortiumId"), #nolint
    consortium = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "consortium"), #nolint
    grantId = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantId")), #nolint
    grantName = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantName")), #nolint
    grantNumber = create_listed_annots(manifest[["grantNumber"]]),
    #publicationId = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "publicationId")), #nolint
    publicationId = NA,
    publicationTitle = create_listed_annots(manifest[["publicationTitle"]]),
    publication = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "pubMedLink", remove_chars = FALSE)), #nolint
  )
}

