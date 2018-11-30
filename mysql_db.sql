CREATE TABLE zt_ic (
    uid TIMESTAMP DEFAULT CURRENT_TIMESTAMP PRIMARY KEY,
    iindex varchar(32),
    inum int not null,
    month01 int default 0,
    month02 int default 0,
    month03 int default 0,
    month04 int default 0,
    month05 int default 0,
    month06 int default 0,
    month07 int default 0,
    month08 int default 0,
    month09 int default 0,
    month10 int default 0,
    month11 int default 0,
    month12 int default 0,
    op   int default 0,
    point1 int default 0,
    point2 int default 0,
    point3 int default 0
  ) ENGINE=innodb;

CREATE TABLE zt_if (
    uid TIMESTAMP DEFAULT CURRENT_TIMESTAMP PRIMARY KEY,
    iindex varchar(32),
    inum int not null,
    month01 int default 0,
    month02 int default 0,
    month03 int default 0,
    month04 int default 0,
    month05 int default 0,
    month06 int default 0,
    month07 int default 0,
    month08 int default 0,
    month09 int default 0,
    month10 int default 0,
    month11 int default 0,
    month12 int default 0,
    op   int default 0,
    point1 int default 0,
    point2 int default 0,
    point3 int default 0
  ) ENGINE=innodb;

  CREATE TABLE zt_ih (
      uid TIMESTAMP DEFAULT CURRENT_TIMESTAMP PRIMARY KEY,
      iindex varchar(32),
      inum int not null,
      month01 int default 0,
      month02 int default 0,
      month03 int default 0,
      month04 int default 0,
      month05 int default 0,
      month06 int default 0,
      month07 int default 0,
      month08 int default 0,
      month09 int default 0,
      month10 int default 0,
      month11 int default 0,
      month12 int default 0,
      op   int default 0,
      point1 int default 0,
      point2 int default 0,
      point3 int default 0
    ) ENGINE=innodb;