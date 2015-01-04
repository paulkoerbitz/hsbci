# HsBCI [![Build Status](https://travis-ci.org/paulkoerbitz/hsbci.svg?branch=master)](https://travis-ci.org/paulkoerbitz/hsbci) [![Coverage Status](https://img.shields.io/coveralls/paulkoerbitz/hsbci.svg)](https://coveralls.io/r/paulkoerbitz/hsbci)

HsBCI aims to be a simple, modular and fast library which implements
the German [*Home Banking Computer Interface* (HBCI) / *Financial
Transaction Services* (FinTS) protocol](http://www.hbci-zka.de/).

## Status

HsBCI is in alpha status and under heavy development. A few things work
for some banks (like getting the balance and statement list), but there
many things do not, most are not tested. Things are not stable and will
likely break.

## ToDos

- Implement missing HBCI Jobs:
  - Transfer
  - TanMediaList
  - ...

- Work with multiple HBCI versions (currently only HBCI Plus)

- Handle errors correctly

- Handle different Tan modes correctly

- Check messages for message size and max num GV before sending

- Correctly extract allowed tan modes from responses