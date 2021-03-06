-- Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
--
-- This library is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version, with the OCaml static compilation exception.
--
-- This library is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
-- License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this library.  If not, see <http://www.gnu.org/licenses/>.

-- @create
CREATE TABLE test (
    id integer PRIMARY KEY,
    exact boolean NOT NULL,
    x float NOT NULL,
    note text
);

-- @insert
INSERT INTO test (exact, x, note) VALUES (?, ?, ?);
-- @set_value
UPDATE test SET x = ? WHERE id = ?;
-- @set_note
UPDATE test SET note = ? WHERE id = ?;
-- @select_all
SELECT * FROM test;
-- @select_ge
SELECT * FROM test WHERE x >= ?;
-- @select_first
SELECT * FROM test LIMIT 1;
-- @count_all
SELECT count(*) FROM test;
-- @count_ge
SELECT count(*) FROM test WHERE x >= ?;
-- @sum_sqr_all
SELECT sum(x * x) FROM test;
-- @sum_sqr_ge
SELECT sum(x * x) FROM test WHERE x >= ?;
