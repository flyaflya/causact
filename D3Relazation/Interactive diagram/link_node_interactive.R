library(jsonlite)

r2d3::r2d3(data = jsonlite::read_json("jsonf.json"), d3_version = 4,
     script = "linknode.js")
