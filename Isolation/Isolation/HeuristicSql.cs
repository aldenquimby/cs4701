using System;
using System.Collections.Generic;
using MySql.Data.MySqlClient;

namespace Isolation
{
    public class HeuristicSql
    {
        #region singleton

        private static readonly Lazy<HeuristicSql> Singleton = new Lazy<HeuristicSql>(() => new HeuristicSql());
        public static HeuristicSql I { get { return Singleton.Value; } }

        #endregion

        private const string ConnString = @"Datasource=localhost;Database=isolation;uid=root;pwd=passwerd;";

        public IList<HeuristicDto> GetHeuristicCache()
        {
            var heuristics = new List<HeuristicDto>();

            const string query = @"
            SELECT Board, Heuristic, Score
            FROM   HeuristicCache;";

            try
            {
                using (var conn = new MySqlConnection(ConnString))
                using (var cmd = new MySqlCommand(query, conn))
                {
                    conn.Open();

                    using (var rdr = cmd.ExecuteReader())
                    {
                        while (rdr.Read())
                        {
                            heuristics.Add(new HeuristicDto
                                {
                                    Board = rdr.GetString("Board"),
                                    Heuristic = rdr.GetString("Heuristic"),
                                    Score = rdr.GetInt32("Score"),
                                });
                        }
                    }
                }
            }
            catch
            {
                Console.WriteLine("Failed to load cache.");
            }

            return heuristics;
        }

        public void SaveHeuristicCache(IList<HeuristicDto> heuristics)
        {
            const string query = @"
            INSERT INTO HeuristicCache
                (Board, Heuristic, Score) VALUES
                (@board, @heuristic, @score)
            ON DUPLICATE KEY UPDATE
                Score = @score;";

            try
            {
                using (var conn = new MySqlConnection(ConnString))
                using (var cmd = new MySqlCommand(query, conn))
                {
                    conn.Open();

                    foreach (var heuristic in heuristics)
                    {
                        cmd.Parameters.Clear();
                        cmd.Parameters.AddWithValue("board", heuristic.Board);
                        cmd.Parameters.AddWithValue("heuristic", heuristic.Heuristic);
                        cmd.Parameters.AddWithValue("score", heuristic.Score);

                        cmd.ExecuteNonQuery();
                    }
                }
            }
            catch
            {
                Console.WriteLine("Failed to save cache.");
            }
        }
    }

    // data transfer object for loading in/out of database
    public class HeuristicDto
    {
        public string Board { get; set; }
        public string Heuristic { get; set; }
        public int Score { get; set; }
    }
}
