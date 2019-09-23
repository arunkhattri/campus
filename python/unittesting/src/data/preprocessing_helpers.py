def convert_to_int(integer_string_with_commas):
    """
    Convert strings with commas to integer without commas

    Parameters
    ----------
    integer_string_with_commas: string(int) with commas

    Returns
    -------
    int
    """
    comma_seperated_parts = integer_string_with_commas.split(",")
    for i in range(len(comma_seperated_parts)):
        if len(comma_seperated_parts[i]) > 3:
            return None
        if i != 0 and len(comma_seperated_parts[i]) != 3:
            return None
    integer_string_without_commas = "".join(comma_seperated_parts)
    try:
        return int(integer_string_without_commas)
    except ValueError:
        return None


def row_to_list(row):
    """
    Convert string to list splitting on "\t",
    if len of list is > 2, returns None.

    Parameters
    ----------
    row: string

    Returns
    -------
    list [string, string]
    
    """
    row = row.strip("\n")
    separated_entries = row.split("\t")
    if len(separated_entries) == 2 and "" not in separated_entries:
        return separated_entries
    return None


def preprocess(raw_data, clean_data):
    """
    Preprocess the data and write cleaned data

    Parameters
    ----------
    raw_data: file path of raw_data to read
    clean_data: file path for cleaned data to write

    Returns
    -------
    txt file with tab separated entries
    """
    with open(raw_data, "r") as f:
        rows = f.readlines()
    with open(clean_data, "w") as out_file:
        for row in rows:
            row_as_list = row_to_list(row)
            if row_as_list is None:
                continue
            area = convert_to_int(row_as_list[0])
            price = convert_to_int(row_as_list[1])
            if area is None and price is None:
                continue
            out_file.write(f"{area}\t{price}\n")
