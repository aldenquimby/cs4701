DROP TABLE IF EXISTS `HeuristicCache` ;

CREATE TABLE IF NOT EXISTS `HeuristicCache` 
(
  `Player` BIT(1) NOT NULL ,  
  `Heuristic` VARCHAR(64) NOT NULL ,
  `Board` VARCHAR(64) NOT NULL ,
  `Score` INT NOT NULL ,
  PRIMARY KEY (`Player`, `Heuristic`, `Board`)
)
ENGINE = InnoDB;

