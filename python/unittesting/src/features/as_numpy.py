# pylint: disable=missing-docstring, invalid-name
import numpy as np


def get_data_as_numpy_array(clean_data, num_cols):
    """
    Returns tab seperated 2 columns data as numpy array.
    """
    result = np.empty((0, num_cols))
    with open(clean_data, "r") as f:
        rows = f.readlines()
        for ind, elem in enumerate(rows):
            try:
                row = np.array([elem.rstrip("\n").split("\t")], dtype=float)
            except ValueError:
                msg = f"Line {ind + 1} of {clean_data} is badly formatted."
                raise ValueError(msg)
            else:
                if row.shape != (1, num_cols):
                    msg = (f"Line {ind + 1} of {clean_data} doesn not have {num_cols} columns")
                    raise ValueError(msg)
            result = np.append(result, row, axis=0)
    return result
