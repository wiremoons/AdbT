[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/hyperium/hyper/master/LICENSE) ![adbt](https://github.com/wiremoons/adbt/workflows/adbt/badge.svg?branch=main) <a href="https://project-types.github.io/#toy">
  <img src="https://img.shields.io/badge/project%20type-toy-blue" alt="Toy Badge"/>
</a>

# What is 'adbt'?

Description : CLI tool to manage an SQlite database of acronyms.   

A small command line application called '`adbt`' (which is an acronym for
'*Acronym DataBase Tool*') which can be used to store, look up, change, or
delete acronyms that are kept in a SQLite database.

The program is small, fast, and is free software. It is used on a daily basis by
the author, running it on both Linux and Windows operating systems. It should
also compile and run on BSD Unix too, and Mac OS X as well, although this has
not been tested.


## Status

**INCOMPLETE** : STILL BEING PORTED FROM C LANGUAGE VERSION

A future version of '`adbt`' will be functionally feature complete, based on its
ability to provide **CRUD**. This is a set of basic features which includes:

 - *CREATE* : new records can be added (ie created) in the database;
 - *READ* : existing records can be searched for (ie read) from the database;
 - *UPDATE*: existing records held in the database can be altered (ie changed);
 - *DELETE*: existing records held in the database can be removed (ie deleted).

This does not mean the program is fully completed (or bug free) - but that it
provides a basic set of functionality. The program is used on a daily basis by
the author, and will continue to be improved as is felt to be necessary.


## Usage Examples

Running `adbt` without any parameters, but with a database available will
output the following information:

```
coming soon...
```

Running `adbt -h` displays the help screen which will output the following
information:

```
coming soon...
```

## Building the Application

An Ada language compiler will be needed to build the application. There are also a
few dependencies that will need to be met for a successful build of `adbt`.
These steps are explained below.

### Dependencies

To be provided.


### Install an Ada Compiler and Supporting Libraries

Install an Ada compiler and supporting libraries. More information on installing 
Ada can be found here: [Get Ada](http://www.getadanow.com/).


### Building 'adbt'

To build `adbt` from source, the following steps can be used:

1. Once Ada is installed on your system, you can clone this GitHub repo with 
the `git` command: `git clone https://github.com/wiremoons/adbt.git`
2. Then in the cloned code directory for `adbt` use `gprbuild` to build a 
release version of `adbt` with the command: `gprbuild -XBUILD=release`. 
Other `gprbuild` build commands include a debug build: `gprbuild -XBUILD=debug`. 
Alternatively, the new (in beta) [Alire](https://alire.ada.dev/) package manager 
should also support the install and build as well.
3. The compiled binary of `adbt` can now be found in the `./bin` sub 
directory. Just copy it somewhere in your path, and it should work when run.

**NOTE:** The build is statically compiled - so the program should run when moved 
to a similar CPU based Linux system, without the Ada standard libraries being 
installed as additional dependencies.

## Database Location

The SQLite database used to store the acronyms can be located in the same
directory as the programs executable. The default filename that is looked for
by the program is: '***acronyms.db***'

However this can be overridden, by giving a preferred location, which can be
specified by an environment variable called ***ACRODB***. You should set this
to the path and preferred database file name of your SQLite acronyms database.
Examples of how to set this for different operating systems are shown below.

On Linux and similar operating systems when using bash shell, add this line to
your `.bashrc` configuration file, located in your home directory (ie
*~/.bashrc*), just amend the path and database file name to suit your own needs:

```
export ACRODB=$HOME/work/my-own.db
```

on Windows or Linux when using Microsoft Powershell:

```
$env:ACRODB += "c:\users\simon\work\my-own.db"
```

on Windows when using a cmd.exe console:

```
set ACRODB=c:\users\simon\work\my-own.db
```

or Windows to add persistently to your environment run the following in a
cmd.exe console:

```
setx ACRODB=c:\users\simon\work\my-own.db
```

## Database and Acronyms Table Setup

**NOTE:** More detailed information is to be added here - plus see point 1 in
todo list below.

SQLite Table used by the program is created with:

```
CREATE TABLE Acronyms ("Acronym","Definition","Description","Source");
```
As long as the same table name and column names are used, the program should
function with an empty database.

With the SQLite command line application `sqlite3` (or `sqlite3.exe` on
Windows) you can create a new database and add a new record using a terminal
window and running the following commands:

```
sqlite3 acronyms.db

CREATE TABLE Acronyms ("Acronym", "Definition", "Description", "Source");

INSERT INTO ACRONYMS(Acronym,Definition,Description,Source) values 
("adbt","Acronym DataBase Tool",
"Command line application to manage a database of acronyms.","Misc");

.quit
```


## Todo ideas and Future Development Plans

Below are some ideas that I am considering adding to the program, in no
particular priority order.

1. Offer to create a new default database if one is not found on start up
2. Ability to populate the database from a remote source
3. Ability to update and/or check for a new version of the program
4. Output of records in different fomats (json, csv, etc)
5. Ability to backup database
6. Ability to backup table within database and keep older versions
7. Tune and add an index to the database
8. Merge contents of different databases that have been updated on separate computer to keep in sync


## Known Issues

Below are issues known to affect `adbt` currently:

- none captured yet - in development.


## Licenses

The following licenses apply to the `adbt` source code, and resulting built
application, as described below.

#### License for 'adbt'

This program `adbt` is licensed under the **MIT License** see
http://opensource.org/licenses/mit for more details.

#### License for 'SQLite'

The SQLite database code used in this application is licensed as **Public
Domain**, see http://www.sqlite.org/copyright.html for more details.

