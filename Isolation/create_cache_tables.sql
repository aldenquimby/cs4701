DROP TABLE IF EXISTS `HeuristicCache` ;

CREATE TABLE IF NOT EXISTS `HeuristicCache` 
(
  `Board` VARCHAR(64) NULL ,
  `Heuristic` VARCHAR(64) NOT NULL ,
  `Score` INT NOT NULL ,
  PRIMARY KEY (`Board`, `Heuristic`)
)
ENGINE = InnoDB;

