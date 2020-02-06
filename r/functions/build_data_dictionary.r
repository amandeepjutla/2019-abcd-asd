# _build_data_dictionary.r
#
# Adapted from https://github.com/ABCD-STUDY/analysis-nda
# 
# Created 20190906

library(checkpoint)
checkpoint("2019-03-18")

library(curl)
library(jsonlite)
library(tidyverse)

abcd_instruments <- fromJSON(
    "https://ndar.nih.gov/api/datadictionary/v2/datastructure?source=ABCD%20Release%202.0"
)
abcd_instruments$shortName

# The next step downloads the NDA versions of each ABCD data dictionary
# and merges them together into a single data frame.

dd = data.frame()
for (i in 1:length(abcd_instruments$shortName)) {
    inst_name = abcd_instruments$shortName[i]
    inst <- fromJSON(
        paste(
            "https://ndar.nih.gov/api/datadictionary/v2/datastructure/", 
            inst_name, 
            sep=""
        )
    )
    inst = inst$dataElements
    # The alias names for each element name are in a list, concatenate
    # that list into a single string
    aliases = lapply(
        inst$aliases, 
        function(x) { 
            str = ""
            if (length(x) > 0) { 
                for(i in 1:length(x)) { 
                    str = paste(str, x[[i]], sep=" ")
                }
            } 
            trimws(str);
        }
    )

    # Create a new data frame
    nd = data.frame(
        "element_name"=inst$name, 
        "data_type"=inst$type, 
        "description"=inst$description, 
        "value_range"=inst$valueRange, 
        "notes"=inst$notes, 
        "aliases"=unlist(aliases)
    )
    # and merge
    if (dim(dd)[1] == 0) { 
        dd <- nd 
    } 
    else { 
        dd <- merge(dd, nd,all=TRUE) 
    }
    print(
        paste(
            "Merged", 
            inst_name, 
            i,
            "/",
            length(abcd_instruments$shortName),
            sep=" "
        )
    )
}

# The resulting data dictionary will have close to 43,000 entries. Only
# about 38,000 are actually provided by ABCD and contain values.

# Save the merged data dictionary as a spreadsheet.

fname <- paste(
    getwd(),
    "abcd_data_dictionary_20190906.csv", 
    sep="/"
)

print(
    paste(
        "write data dictionary to ",
        fname,sep="")
)
write.csv(dd,fname)
