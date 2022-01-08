# read the contents of your README file
from pathlib import Path

from setuptools import setup

this_directory = Path(__file__).parent
long_description = (this_directory / "README.md").read_text()

setup(
    name="yadasm",
    version="0.1.1",
    packages=[
        "lyadasm",
        "lyadasm.core",
        "lyadasm.cli",
        "lyadasm.test",
        "lyadasm.core.archs",
        "lyadasm.core.middleware",
    ],
    package_data={"": ["README.md", "LICENSE"]},
    author="Lukas Krickl",
    author_email="lukas@krickl.dev",
    url="https://github.com/unlink2/yadasm",
    description="Yet Another Disassembler",
    long_description=long_description,
    long_description_content_type="text/markdown",
    platforms=["any"],
    license="MIT",
    scripts=["yadasm"],
)
