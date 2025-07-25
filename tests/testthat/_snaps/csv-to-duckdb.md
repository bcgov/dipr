# make_sql_schema_string works

    Code
      make_sql_schema_string(db, schema = schema)
    Output
      [1] "{\n    'SERVDT': 'DATE',\n    'PHNNUM': 'VARCHAR',\n    'FITM': 'BIGINT',\n    'PRACNUM': 'VARCHAR',\n    'PAYENUM': 'VARCHAR',\n    'FIRSTPAIDDT': 'TIMESTAMP',\n    'LASTPAIDDT': 'TIMESTAMP',\n    'PAIDSERV': 'BIGINT',\n    'PMEPDAMT': 'REAL',\n    'PMENIAMT': 'REAL'\n  }"

---

    Code
      make_sql_schema_string(db, schema = schema, dt_override = c(LASTPAIDDT = "%Y-%m"))
    Output
      [1] "{\n    'SERVDT': 'DATE',\n    'PHNNUM': 'VARCHAR',\n    'FITM': 'BIGINT',\n    'PRACNUM': 'VARCHAR',\n    'PAYENUM': 'VARCHAR',\n    'FIRSTPAIDDT': 'TIMESTAMP',\n    'LASTPAIDDT': 'VARCHAR',\n    'PAIDSERV': 'BIGINT',\n    'PMEPDAMT': 'REAL',\n    'PMENIAMT': 'REAL'\n  }"

---

    Code
      make_sql_schema_string(db, schema = schema)
    Condition
      Error in `check_duckdb_types()`:
      ! The following are not valid duckdb data types: 'itsabiginteger'

# make_select_statement_works

    Code
      make_select_statement(schema)
    Output
      [1] "SERVDT AS servdt,\n  PHNNUM AS phnnum,\n  FITM AS fitm,\n  PRACNUM AS pracnum,\n  PAYENUM AS payenum,\n  FIRSTPAIDDT AS firstpaiddt,\n  LASTPAIDDT AS lastpaiddt,\n  PAIDSERV AS paidserv,\n  PMEPDAMT AS pmepdamt,\n  PMENIAMT AS pmeniamt"

---

    Code
      make_select_statement(schema, col_rename = c(PHNNUM = "studyid", LASTPAIDDT = "last_paid_date"))
    Output
      [1] "SERVDT AS servdt,\n  PHNNUM AS studyid,\n  FITM AS fitm,\n  PRACNUM AS pracnum,\n  PAYENUM AS payenum,\n  FIRSTPAIDDT AS firstpaiddt,\n  LASTPAIDDT AS last_paid_date,\n  PAIDSERV AS paidserv,\n  PMEPDAMT AS pmepdamt,\n  PMENIAMT AS pmeniamt"

---

    Code
      make_select_statement(schema, col_rename = c(PHNNUM = "studyid", LASTPAIDDT = "last_paid_date"),
      dt_override = c(LASTPAIDDT = "%Y-%m"))
    Output
      [1] "SERVDT AS servdt,\n  PHNNUM AS studyid,\n  FITM AS fitm,\n  PRACNUM AS pracnum,\n  PAYENUM AS payenum,\n  FIRSTPAIDDT AS firstpaiddt,\n  strptime(LASTPAIDDT, '%Y-%m') AS last_paid_date,\n  PAIDSERV AS paidserv,\n  PMEPDAMT AS pmepdamt,\n  PMENIAMT AS pmeniamt"

# csv_to_duckdb_query works

    Code
      csv_to_duckdb_query(db, "foo.csv", "tbl", schema, col_rename = c(PHNNUM = "studyid",
        LASTPAIDDT = "last_paid_date"), date_format = "%Y/%m/%d", timestamp_format = "%Y-%m%d $H:%M:%S",
      dt_override = c(LASTPAIDDT = "%Y-%m"))
    Output
      CREATE OR REPLACE TABLE tbl AS
      SELECT
        SERVDT AS servdt,
        PHNNUM AS studyid,
        FITM AS fitm,
        PRACNUM AS pracnum,
        PAYENUM AS payenum,
        FIRSTPAIDDT AS firstpaiddt,
        strptime(LASTPAIDDT, '%Y-%m') AS last_paid_date,
        PAIDSERV AS paidserv,
        PMEPDAMT AS pmepdamt,
        PMENIAMT AS pmeniamt
      FROM read_csv(
        'foo.csv',
        types = {
          'SERVDT': 'DATE',
          'PHNNUM': 'VARCHAR',
          'FITM': 'BIGINT',
          'PRACNUM': 'VARCHAR',
          'PAYENUM': 'VARCHAR',
          'FIRSTPAIDDT': 'TIMESTAMP',
          'LASTPAIDDT': 'VARCHAR',
          'PAIDSERV': 'BIGINT',
          'PMEPDAMT': 'REAL',
          'PMENIAMT': 'REAL'
        },
        dateformat = '%Y/%m/%d',
        timestampformat = '%Y-%m%d $H:%M:%S'
      )

---

    Code
      csv_to_duckdb_query(db, "foo.csv", "tbl", schema, col_rename = c(PHNNUM = "studyid",
        LASTPAIDDT = "last_paid_date"), date_format = "%Y/%m/%d", timestamp_format = "%Y-%m%d $H:%M:%S",
      dt_override = list(FIRSTPAIDDT = c("%Y-m%", "%Y%m%d %h%M%S"), LASTPAIDDT = "%Y-%m"))
    Output
      CREATE OR REPLACE TABLE tbl AS
      SELECT
        SERVDT AS servdt,
        PHNNUM AS studyid,
        FITM AS fitm,
        PRACNUM AS pracnum,
        PAYENUM AS payenum,
        strptime(FIRSTPAIDDT, ['%Y-m%','%Y%m%d %h%M%S']) AS firstpaiddt,
        strptime(LASTPAIDDT, '%Y-%m') AS last_paid_date,
        PAIDSERV AS paidserv,
        PMEPDAMT AS pmepdamt,
        PMENIAMT AS pmeniamt
      FROM read_csv(
        'foo.csv',
        types = {
          'SERVDT': 'DATE',
          'PHNNUM': 'VARCHAR',
          'FITM': 'BIGINT',
          'PRACNUM': 'VARCHAR',
          'PAYENUM': 'VARCHAR',
          'FIRSTPAIDDT': 'VARCHAR',
          'LASTPAIDDT': 'VARCHAR',
          'PAIDSERV': 'BIGINT',
          'PMEPDAMT': 'REAL',
          'PMENIAMT': 'REAL'
        },
        dateformat = '%Y/%m/%d',
        timestampformat = '%Y-%m%d $H:%M:%S'
      )

---

    Code
      csv_to_duckdb_query(db, c("foo.csv", "bar.csv"), "tbl", schema)
    Output
      CREATE OR REPLACE TABLE tbl AS
      SELECT
        SERVDT AS servdt,
        PHNNUM AS phnnum,
        FITM AS fitm,
        PRACNUM AS pracnum,
        PAYENUM AS payenum,
        FIRSTPAIDDT AS firstpaiddt,
        LASTPAIDDT AS lastpaiddt,
        PAIDSERV AS paidserv,
        PMEPDAMT AS pmepdamt,
        PMENIAMT AS pmeniamt
      FROM read_csv(
        ['foo.csv','bar.csv'],
        types = {
          'SERVDT': 'DATE',
          'PHNNUM': 'VARCHAR',
          'FITM': 'BIGINT',
          'PRACNUM': 'VARCHAR',
          'PAYENUM': 'VARCHAR',
          'FIRSTPAIDDT': 'TIMESTAMP',
          'LASTPAIDDT': 'TIMESTAMP',
          'PAIDSERV': 'BIGINT',
          'PMEPDAMT': 'REAL',
          'PMENIAMT': 'REAL'
        },
        dateformat = '%Y%m',
        timestampformat = '%Y%m%d%H%M'
      )

---

    Code
      csv_to_duckdb_query(db, "foo.csv", "tbl", schema, nullstr = "NULL")
    Output
      CREATE OR REPLACE TABLE tbl AS
      SELECT
        SERVDT AS servdt,
        PHNNUM AS phnnum,
        FITM AS fitm,
        PRACNUM AS pracnum,
        PAYENUM AS payenum,
        FIRSTPAIDDT AS firstpaiddt,
        LASTPAIDDT AS lastpaiddt,
        PAIDSERV AS paidserv,
        PMEPDAMT AS pmepdamt,
        PMENIAMT AS pmeniamt
      FROM read_csv(
        'foo.csv',
        types = {
          'SERVDT': 'DATE',
          'PHNNUM': 'VARCHAR',
          'FITM': 'BIGINT',
          'PRACNUM': 'VARCHAR',
          'PAYENUM': 'VARCHAR',
          'FIRSTPAIDDT': 'TIMESTAMP',
          'LASTPAIDDT': 'TIMESTAMP',
          'PAIDSERV': 'BIGINT',
          'PMEPDAMT': 'REAL',
          'PMENIAMT': 'REAL'
        },
        dateformat = '%Y%m',
        timestampformat = '%Y%m%d%H%M',
        nullstr = 'NULL'
      )

---

    Code
      csv_to_duckdb_query(db, "foo.csv", "tbl", schema, nullstr = c("NULL", "NA", ""))
    Output
      CREATE OR REPLACE TABLE tbl AS
      SELECT
        SERVDT AS servdt,
        PHNNUM AS phnnum,
        FITM AS fitm,
        PRACNUM AS pracnum,
        PAYENUM AS payenum,
        FIRSTPAIDDT AS firstpaiddt,
        LASTPAIDDT AS lastpaiddt,
        PAIDSERV AS paidserv,
        PMEPDAMT AS pmepdamt,
        PMENIAMT AS pmeniamt
      FROM read_csv(
        'foo.csv',
        types = {
          'SERVDT': 'DATE',
          'PHNNUM': 'VARCHAR',
          'FITM': 'BIGINT',
          'PRACNUM': 'VARCHAR',
          'PAYENUM': 'VARCHAR',
          'FIRSTPAIDDT': 'TIMESTAMP',
          'LASTPAIDDT': 'TIMESTAMP',
          'PAIDSERV': 'BIGINT',
          'PMEPDAMT': 'REAL',
          'PMENIAMT': 'REAL'
        },
        dateformat = '%Y%m',
        timestampformat = '%Y%m%d%H%M',
        nullstr = ['NULL', 'NA', '']
      )

---

    Code
      csv_to_duckdb_query(db, "foo.csv", "tbl", schema, nullstr = c("NULL", "NA", ""),
      opts = list(comment = "#", force_not_null = c("a", "b"), null_padding = TRUE,
      buffer_size = 5))
    Output
      CREATE OR REPLACE TABLE tbl AS
      SELECT
        SERVDT AS servdt,
        PHNNUM AS phnnum,
        FITM AS fitm,
        PRACNUM AS pracnum,
        PAYENUM AS payenum,
        FIRSTPAIDDT AS firstpaiddt,
        LASTPAIDDT AS lastpaiddt,
        PAIDSERV AS paidserv,
        PMEPDAMT AS pmepdamt,
        PMENIAMT AS pmeniamt
      FROM read_csv(
        'foo.csv',
        types = {
          'SERVDT': 'DATE',
          'PHNNUM': 'VARCHAR',
          'FITM': 'BIGINT',
          'PRACNUM': 'VARCHAR',
          'PAYENUM': 'VARCHAR',
          'FIRSTPAIDDT': 'TIMESTAMP',
          'LASTPAIDDT': 'TIMESTAMP',
          'PAIDSERV': 'BIGINT',
          'PMEPDAMT': 'REAL',
          'PMENIAMT': 'REAL'
        },
        dateformat = '%Y%m',
        timestampformat = '%Y%m%d%H%M',
        comment = '#',
        nullstr = ['NULL', 'NA', ''],
        force_not_null = ['a', 'b'],
        null_padding = true,
        buffer_size = 5
      )

# parse_opts works

    Code
      parse_opts(db, NULL, NULL)
    Condition
      Error in `parse_opts()`:
      ! is.list(opts) is not TRUE

---

    Code
      parse_opts(db, list(), NULL)
    Condition
      Error in `parse_opts()`:
      ! is.list(opts) is not TRUE

---

    Code
      parse_opts(db, nullstr = c("NA", "NULL"), opts = list(comment = "#",
        force_not_null = c("a", "b"), null_padding = TRUE, buffer_size = 5))
    Output
      [1] ",\n  comment = '#',\n  nullstr = ['NA', 'NULL'],\n  force_not_null = ['a', 'b'],\n  null_padding = true,\n  buffer_size = 5"

---

    Code
      parse_opts(db, opts = list(comment = "#", buffer_size = 5))
    Output
      [1] ",\n  comment = '#',\n  buffer_size = 5"

---

    Code
      parse_opts(db, opts = list(comment = "#", blahblah = 5))
    Condition
      Error:
      ! Illegal duckdb read_csv parameters: blahblah

