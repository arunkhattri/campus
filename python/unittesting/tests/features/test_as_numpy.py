# pylint: disable=missing-docstring, invalid-name, wrong-import-position
import os
import pytest
import numpy as np
import sys

sys.path.append("/home/arunkhattri/github/campus/python/unittesting/src/")
from features.as_numpy import get_data_as_numpy_array


@pytest.fixture
def clean_data():
    file_path = "clean.txt"
    with open(file_path, "w") as f:
        f.write("201\t305671\n7892\t298140\n501\t738293\n")
    yield file_path
    os.remove(file_path)


@pytest.fixture
def empty_file():
    file_path = "empty.txt"
    open(file_path, "w").close()
    yield file_path
    os.remove(file_path)


class TestGetDataAsNumpyArray():
    def test_on_clean_file(self, clean_data):
        expected = np.array([[201.0, 305671.0], [7892.0, 298140.0], [501.0, 738293.0]])
        actual = get_data_as_numpy_array(clean_data, 2)
        msg = f"Expected: {expected}; Actual: {actual}"
        assert actual == pytest.approx(expected), msg

    def test_on_empty_file(self, empty_file):
        expected = np.empty((0, 2))
        actual = get_data_as_numpy_array(empty_file, 2)
        msg = f"Expected: {expected}; Actual: {actual}"
        assert actual == pytest.approx(expected), msg
