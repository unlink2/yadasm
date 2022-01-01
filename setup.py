from setuptools import setup

setup(
    name="yadasm",
    version="0.1.0",
    packages=["cli", "core", "."],
    package_data={"": ["README.md", "LICENSE"]},
    author="Lukas Krickl",
    author_email="lukas@krickl.dev",
    url="https://github.com/unlink2/yadasm",
    description="",
    platforms=["any"],
    license="MIT",
    scripts=["yadasm.py"],
)
