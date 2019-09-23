# pylint: disable= missing-docstring
from setuptools import setup, find_packages

setup(name="unittesting",
      version="0.1.0",
      description="learning unit testing from datacamp",
      author="Arun Kr. Khattri",
      packages=find_packages("src"),
      package_dir={"": "src"},
      author_email="arun.kr.khattri@gmail.com",
      install_requires=["jupyter",
                        "matplotlib",
                        "pytest",
                        "pytest-mpl",
                        "pytest-mock",
                        "scipy",
                        "numpy"],)
