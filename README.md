# This is ActivityLog2, a fitness activity tracker.

The application can import data from activities such as swim, bike and run,
and display various plots and reports on them.  Data is imported from .FIT
files, which is a popular data format used by many fitness devices.

Data from the application can be exported as CSV, to be further analyzed by
other tools, or, if you know Racket, you can easily use various data
manipulations from Racket REPL itself.  You don't need to know Racket to be
able to use the application and view the built-in plots.

For more details, see http://alex-hhh.github.io/ActivityLog2/

To run the application, you will first need to install Racket from
http://www.racket-lang.org.  Once Racket is installed, open the file "run.rkt"
and select "Racket/Run" (Ctrl-R).  You will be prompted to open an existing
database or create a new one.  Once you have a database, you can import FIT
files from Garmin (and other devices).  For more details on how to build a
stand alone executable, see the [doc/building.md](doc/building.md) file.

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.
