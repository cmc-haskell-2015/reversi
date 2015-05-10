# reversi

[![Build Status](https://travis-ci.org/cmc-haskell-2015/reversi.svg?branch=master)](https://travis-ci.org/cmc-haskell-2015/reversi)

Игра «Реверси».

## Установка

Для установки клонируйте репозиторий и запустите `cabal install`:

```
$ git clone https://github.com/cmc-haskell-2015/reversi.git
$ cd reversi
$ cabal install
```

Установка SQLite

Скачать https://www.sqlite.org/download.html 
	
32-bit DLL (x86) for SQLite verison 3.8.10.
(sha1: 3607eac9af97cec3e3d5e5945499cb6b7aab15e7)

Распаковать архив в C:\sqlite

Выполнить
```
$ cd reversi
$ cabal install sqlite --extra-include-dirs=C:\sqlite --extra-lib-dirs
=C:\sqlite
$ cabal install persistent
$ cabal install persistent-hssqlppp
$ cabal install persistent-sqlite
```

(на cabal install persistent-hssqlppp могут быть ошибки -- это норма. Если 
завис на какой-то строчке, ctrl + c посылаем)