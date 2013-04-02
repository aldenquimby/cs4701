﻿using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

namespace Isolation
{
    // Generic isolation board evaluator
    public abstract class HeuristicBase
    {
        public abstract int Evaluate(Board board);
        public abstract string Name { get; }
    }
    
    // Number possible moves for me vs. opponent
    public class NumberOfMovesHeuristic : HeuristicBase
    {
        public override int Evaluate(Board board)
        {
            var myMoveCount = board.GetMyValidMoves().Count();
            var opponenentMoveCount = board.GetOpponentValidMoves().Count();

            if (myMoveCount == 0 && opponenentMoveCount == 0)
            {
                return board.PlayerToMove == board.MyPlayer ? int.MinValue : int.MaxValue;
            }

            if (myMoveCount == 0)
            {
                return int.MinValue;
            }
        
            if (opponenentMoveCount == 0)
            {
                return int.MaxValue;
            }

            return myMoveCount - opponenentMoveCount;
        }

        public override string Name { get { return "NumberOfMoves"; } }
    }

    // Number of empty spaces reachable by me vs. opponent
    public class OpenAreaHeuristic : HeuristicBase
    {
        private IEnumerable<BoardSpace> GetSurroundingSpaces(BoardSpace space)
        {
            if (space.Row > 0)
            {
                yield return new BoardSpace((byte)(space.Row - 1), space.Col);

                if (space.Col > 0)
                {
                    yield return new BoardSpace((byte)(space.Row - 1), (byte)(space.Col - 1));
                    yield return new BoardSpace(space.Row, (byte)(space.Col - 1));
                }
                if (space.Col < 7)
                {
                    yield return new BoardSpace((byte)(space.Row - 1), (byte)(space.Col + 1));
                    yield return new BoardSpace(space.Row, (byte)(space.Col + 1));
                }
            }
            if (space.Row < 7)
            {
                yield return new BoardSpace((byte)(space.Row + 1), space.Col);

                if (space.Col > 0)
                {
                    yield return new BoardSpace((byte)(space.Row + 1), (byte)(space.Col - 1));
                }
                if (space.Col < 7)
                {
                    yield return new BoardSpace((byte)(space.Row + 1), (byte)(space.Col + 1));
                }
            }
        } 

        private HashSet<BoardSpace> GetOpenArea(Board board, Player player)
        {
            var initialPosition = player == Player.X ? board.Xposition : board.Oposition;

            var toExamine = new Queue<BoardSpace>(new []{initialPosition});
            var closed = new HashSet<BoardSpace>{initialPosition};
            var accessible = new HashSet<BoardSpace>();

            while (toExamine.Count > 0)
            {
                var space = toExamine.Dequeue();

                // enqueue empty spaces immediately next to this space
                foreach (var successor in GetSurroundingSpaces(space))
                {
                    // skip spaces we've seen
                    if (closed.Contains(successor))
                    {
                        continue;
                    }

                    // mark this sapce as seen
                    closed.Add(successor);

                    if (board[successor.Row, successor.Col] == BoardSpaceValue.Empty)
                    {
                        accessible.Add(successor);
                        toExamine.Enqueue(successor);
                    }
                }
            }

            return accessible;
        }

        public override int Evaluate(Board board)
        {
            var myOpenArea = GetOpenArea(board, board.MyPlayer);
            var opponentOpenArea = GetOpenArea(board, board.OpponentPlayer);

            if (myOpenArea.Count == 0 && opponentOpenArea.Count == 0)
            {
                return board.PlayerToMove == board.MyPlayer ? int.MinValue : int.MaxValue;
            }

            if (myOpenArea.Count == 0)
            {
                return int.MinValue;
            }
        
            if (opponentOpenArea.Count == 0)
            {
                return int.MaxValue;
            }

            // if we're in completely separate areas, whoever has a bigger area will win
            if (myOpenArea.All(x => !opponentOpenArea.Contains(x)))
            {
                // if areas are the same size, whoever has to move right now will lose
                if (myOpenArea.Count == opponentOpenArea.Count)
                {
                    return board.PlayerToMove == board.MyPlayer ? int.MinValue : int.MaxValue;
                }
                
                return myOpenArea.Count > opponentOpenArea.Count ? int.MaxValue : int.MinValue;
            }

            // since we're in the same area, use current move count
            return board.GetMyValidMoves().Count() - board.GetOpponentValidMoves().Count();

            // use open area size difference
            return myOpenArea.Count - opponentOpenArea.Count;
        }

        public override string Name { get { return "OpenArea"; } }
    }

    // Global static heuristic evaluator
    public class HeuristicCache
    {
        #region singleton

        private static readonly Lazy<HeuristicCache> Singleton = new Lazy<HeuristicCache>(() => new HeuristicCache());
        public static HeuristicCache I { get { return Singleton.Value; } }

        #endregion

        private readonly IDictionary<string, IDictionary<string, int>> _cache;

        public HeuristicCache()
        {
            _cache = new ConcurrentDictionary<string, IDictionary<string, int>>();
        }

        public int Evaluate(Board board, HeuristicBase heuristic)
        {
            if (board == null || heuristic == null)
            {
                return 0;
            }

            if (!_cache.ContainsKey(heuristic.Name))
            {
                _cache[heuristic.Name] = new ConcurrentDictionary<string, int>();
            }

            var boardString = board.ToFlatString();

            if (!_cache[heuristic.Name].ContainsKey(boardString))
            {
                _cache[heuristic.Name][boardString] = heuristic.Evaluate(board);
            }

            return _cache[heuristic.Name][boardString];
        }

        public void LoadCache(IList<HeuristicDto> dtos)
        {
            _cache.Clear();

            foreach (var dto in dtos)
            {
                if (!_cache.ContainsKey(dto.Heuristic))
                {
                    _cache[dto.Heuristic] = new Dictionary<string, int>();
                }

                _cache[dto.Heuristic][dto.Board] = dto.Score;
            }
        }

        public IList<HeuristicDto> DumpCache()
        {
            var heuristics = _cache.SelectMany(heuristic => heuristic.Value, (heuristic, board) => new HeuristicDto
                {
                    Heuristic = heuristic.Key,
                    Score = board.Value,
                    Board = board.Key,
                }).OrderByDescending(x => x.Heuristic).ToList();

            _cache.Clear();

            return heuristics;
        }
    }
}
