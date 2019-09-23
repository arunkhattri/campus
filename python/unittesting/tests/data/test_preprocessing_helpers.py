# pylint: disable=missing-docstring
from unittest.mock import call
import pytest
import sys
sys.path.append("/home/arunkhattri/github/campus/python/unittesting/src/")
# import pprint
# pprint.pprint(sys.path)

from data.preprocessing_helpers import convert_to_int, row_to_list, preprocess


@pytest.fixture
def raw_and_clean_data_file(tmpdir):
    raw_path = tmpdir.join("raw.txt")
    clean_path = tmpdir.join("clean.txt")
    with open(raw_path, "w") as f:
        f.write(
            "1,801\t201,411\n"
            "1,767565,112\n"
            "2,002\t333,209\n"
            "1990\t782,911\n"
            "1,285\t389129\n")
    return raw_path, clean_path


def row_to_list_bug_free(row):
    return_val = {"1,801\t201,411\n": ["1,801", "201,411"],
                  "1,767565,112\n": None,
                  "2,002\t333,209\n": ["2,002", "333,209"],
                  "1990\t782,911\n": ["1990", "782,911"],
                  "1,285\t389129\n": ["1,285", "389129"]}
    return return_val[row]


def convert_to_int_bug_free(comma_separated_integer_string):
    return_values = {"1,801": 1801,
                     "201,411": 201411,
                     "2,002": 2002,
                     "333,209": 333209,
                     "1990": None,
                     "782,911": 782911,
                     "1,285": 1285,
                     "389129": None,
                     }
    return return_values[comma_separated_integer_string]


class TestConvertToInt():
    def test_with_no_comma(self):
        test_arg = "756"
        expected = 756
        actual = convert_to_int(test_arg)
        msg = f"Expected: {expected}, Actual: {actual}"
        assert actual == expected, msg

    def test_with_one_comma(self):
        test_arg = "2,081"
        expected = 2081
        actual = convert_to_int(test_arg)
        msg = f"Expected: {expected}, Actual: {actual}"
        assert actual == expected, msg

    def test_with_two_comma(self):
        test_arg = "1,034,081"
        expected = 1034081
        actual = convert_to_int(test_arg)
        msg = f"Expected: {expected}, Actual: {actual}"
        assert actual == expected, msg
