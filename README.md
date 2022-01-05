
# yadasm

![](https://github.com/unlink2/yadasm/actions/workflows/build.yml/badge.svg)
![](https://github.com/unlink2/yadasm/actions/workflows/test.yml/badge.svg)

## Yet Another Disassembler

Yadasm is a dynamically configurable dissasembler tool.
It is aimed mainly at romhacking and reverse engeneering and intends to be a scriptable
and highly configurable tool that can be applied to any project as needed.

## Table of content

- [Installation](#Installation)
- [Usage](#Usage)
- [License](#License)
- [Contributing](#Contributing)
- [TODO](#TODO)

## Installation

To install yadasm as a pip package first make sure you Wheel is installed
```sh
pip install wheel
```

Then run
```sh
./build.sh
```

This will create a wheel package for you.

Lastly simply install it using

```sh
pip install ./dist/yadasm-0.1.0-py3-none-any.whl
```

## Usage

The yadasm command line is very simple:

```
usage: yadasm [-h] [--arch ARCH] [--file-start-offset FILE_START_OFFSET] [--file-end-offset FILE_END_OFFSET] [--start-addr START_ADDR] [--end-addr END_ADDR] [--label-postfix LABEL_POSTFIX] file

yadasm

positional arguments:
  file                  the input file

options:
  -h, --help            show this help message and exit
  --arch ARCH           The target cpu architecture dict_keys(['6502', '6502-byte', '65c02', '65c02-byte', '65c816', '65c816-byte', '65c816-emu', '65c816-emu-byte'])
  --file-start-offset FILE_START_OFFSET
                        Start parsing input file at offset
  --file-end-offset FILE_END_OFFSET
                        Stop parsing input file at offset
  --start-addr START_ADDR
                        Starting address
  --end-addr END_ADDR   End addres
  --label-postfix LABEL_POSTFIX
```

However yadasm can also be used as a library which allows precies configuration
of the dissassembly output.

```py
import yadasm
```

TODO further library documentation

## License

This program is distributed under the terms of the MIT License.

## Contributing

All contributions are welcome.
Both pull requests and issue reports are always appreciated.
Please make sure that all existing tests pass before submitting a pull request.
The code is formatted using [black](https://github.com/psf/black).
You can execute the entire code quality test suite using the `check.sh` script.

## TODO
- context needs to keep track of "seen" addresses/line numbers and add labels accordingly
