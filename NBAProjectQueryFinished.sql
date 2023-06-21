-- Clean up the NBA_Team_Stats table
UPDATE dbo.NBA_Team_Stats
SET Year = LEFT(Year, 4);

-- Cleaned NBA_Player_Stats_2 table
SELECT
  Rk,
  Player,
  Tm,
  G,
  MP,
  FG,
  FGA,
  FG1,
  _3P,
  _3PA,
  _3P1,
  _2P,
  _2PA,
  _2P1,
  eFG,
  Season,
  CASE WHEN Rank() OVER (PARTITION BY Season ORDER BY _3P DESC) <= 10 THEN 1 ELSE 0 END AS IsTop10In3P
INTO dbo.Cleaned_NBA_Player_Stats
FROM dbo.NBA_Player_Stats_2
WHERE Season >= '2001' AND Player NOT IN (
    SELECT Player
    FROM dbo.NBA_Player_Stats_2
    GROUP BY Player, Season
    HAVING COUNT(*) >= 2
) AND G >= 50;

-- Update the columns and format the values in the Cleaned_NBA_Player_Stats table
UPDATE dbo.Cleaned_NBA_Player_Stats
SET MP = FORMAT(MP, 'N2'),
    FG = FORMAT(FG, 'N2'),
    FGA = FORMAT(FGA, 'N2'),
    FG1 = FORMAT(FG1, 'N2'),
    _3P = FORMAT(_3P, 'N2'),
    _3PA = FORMAT(_3PA, 'N2'),
    _3P1 = FORMAT(_3P1, 'N2'),
    _2P = FORMAT(_2P, 'N2'),
    _2PA = FORMAT(_2PA, 'N2'),
    _2P1 = FORMAT(_2P1, 'N2'),
    eFG = FORMAT(eFG, 'N2'),
    Season = LEFT(Season, 4);

-- Drop the temporary table
DROP TABLE dbo.NBA_Player_Stats_2;

-- Update team names in Cleaned_NBA_Player_Stats
UPDATE dbo.Cleaned_NBA_Player_Stats
SET Tm =
  CASE
    WHEN Tm = 'ATL' THEN 'Atlanta'
    WHEN Tm = 'BOS' THEN 'Boston'
    WHEN Tm = 'BRK' THEN 'Brooklyn'
    WHEN Tm = 'CHA' THEN 'Charlotte'
    WHEN Tm = 'CHH' THEN 'Charlotte'
    WHEN Tm = 'CHI' THEN 'Chicago'
    WHEN Tm = 'CHO' THEN 'Charlotte'
    WHEN Tm = 'CLE' THEN 'Cleveland'
    WHEN Tm = 'DAL' THEN 'Dallas'
    WHEN Tm = 'DEN' THEN 'Denver'
    WHEN Tm = 'DET' THEN 'Detroit'
    WHEN Tm = 'GSW' THEN 'Golden State'
    WHEN Tm = 'IND' THEN 'Indiana'
    WHEN Tm = 'HOU' THEN 'Houston'
    WHEN Tm = 'L.A. Lakers' THEN 'L.A. Lakers'
    WHEN Tm = 'L.A. Clippers' THEN 'L.A. Clippers'
    WHEN Tm = 'MEM' THEN 'Memphis'
    WHEN Tm = 'MIA' THEN 'Miami'
    WHEN Tm = 'MIL' THEN 'Milwaukee'
    WHEN Tm = 'MIN' THEN 'Minnesota'
    WHEN Tm = 'NJN' THEN 'New Jersey'
    WHEN Tm = 'NOH' THEN 'New Orleans'
    WHEN Tm = 'NOP' THEN 'New Orleans'
    WHEN Tm = 'NOK' THEN 'New Orleans'
    WHEN Tm = 'NYK' THEN 'New York'
    WHEN Tm = 'OKC' THEN 'Oklahoma City'
    WHEN Tm = 'ORL' THEN 'Orlando'
    WHEN Tm = 'PHI' THEN 'Philadelphia'
    WHEN Tm = 'PHO' THEN 'Phoenix'
    WHEN Tm = 'POR' THEN 'Portland'
    WHEN Tm = 'SAS' THEN 'San Antonio'
    WHEN Tm = 'SAC' THEN 'Sacramento'
    WHEN Tm = 'SEA' THEN 'Seattle'
    WHEN Tm = 'TOR' THEN 'Toronto'
    WHEN Tm = 'TOT' THEN 'Toronto'
    WHEN Tm = 'UTA' THEN 'Utah'
    WHEN Tm = 'VAN' THEN 'Vancouver'
    WHEN Tm = 'WAS' THEN 'Washington'
    ELSE Tm
  END;

-- Merge Cleaned_NBA_Player_Stats and NBA_Team_Stats into Team_Player_Stats_Merged
SELECT
  P.Player,
  P.IsTop10In3P,
  T.*
INTO dbo.Team_Player_Stats_Merged
FROM dbo.NBA_Team_Stats AS T
LEFT JOIN dbo.Cleaned_NBA_Player_Stats AS P ON T.Team = P.Tm AND T.Year = P.Season;

-- Select the merged data from Team_Player_Stats_Merged
SELECT *
FROM dbo.Team_Player_Stats_Merged
WHERE Year >= 2001;