using System;
using System.Collections.Generic;

namespace Isolation
{
    public class MoveGenerator
    {
        #region singleton

        private static readonly Lazy<MoveGenerator> Singelton = new Lazy<MoveGenerator>(() => new MoveGenerator());
        public static MoveGenerator I { get { return Singelton.Value; } }

        #endregion

        public List<BoardSpace> GetMovesForX(Board board)
        {
            var moves = new List<BoardSpace>();

            #region vertical moves

            for (var i = (byte)(board.Xposition.Row + 1); i < 8; i++)
            {
                if (board[i, board.Xposition.Col] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, board.Xposition.Col));
                }
                else { break; }
            }

            for (var i = (byte)(board.Xposition.Row - 1); i < 8; i--) // byte wraps to 255
            {
                if (board[i, board.Xposition.Col] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, board.Xposition.Col));
                }
                else { break; }
            }

            #endregion

            #region horizontal moves

            for (var j = (byte)(board.Xposition.Col + 1); j < 8; j++)
            {
                if (board[board.Xposition.Row, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(board.Xposition.Row, j));
                }
                else { break; }
            }

            for (var j = (byte)(board.Xposition.Col - 1); j < 8; j--) // byte wraps to 255
            {
                if (board[board.Xposition.Row, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(board.Xposition.Row, j));
                }
                else { break; }
            }

            #endregion

            #region diagonal moves

            for (byte i = (byte)(board.Xposition.Row + 1), j = (byte)(board.Xposition.Col + 1); i < 8 && j < 8; i++, j++)
            {
                if (board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, j));
                }
                else { break; }
            }

            for (byte i = (byte)(board.Xposition.Row - 1), j = (byte)(board.Xposition.Col - 1); i < 8 && j < 8; i--, j--)
            {
                if (board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, j));
                }
                else { break; }
            }

            #endregion

            return moves;
        }

        public List<BoardSpace> GetMovesForO(Board board)
        {
            var moves = new List<BoardSpace>();

            #region vertical moves

            for (var i = (byte)(board.Xposition.Row + 1); i < 8; i++)
            {
                if (board[i, board.Xposition.Col] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, board.Xposition.Col));
                }
                else { break; }
            }

            for (var i = (byte)(board.Xposition.Row - 1); i < 8; i--) // byte wraps to 255
            {
                if (board[i, board.Xposition.Col] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, board.Xposition.Col));
                }
                else { break; }
            }

            #endregion

            #region horizontal moves

            for (var j = (byte)(board.Xposition.Col + 1); j < 8; j++)
            {
                if (board[board.Xposition.Row, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(board.Xposition.Row, j));
                }
                else { break; }
            }

            for (var j = (byte)(board.Xposition.Col - 1); j < 8; j--) // byte wraps to 255
            {
                if (board[board.Xposition.Row, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(board.Xposition.Row, j));
                }
                else { break; }
            }

            #endregion

            #region diagonal moves

            for (byte i = (byte)(board.Xposition.Row + 1), j = (byte)(board.Xposition.Col + 1); i < 8 && j < 8; i++, j++)
            {
                if (board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, j));
                }
                else { break; }
            }

            for (byte i = (byte)(board.Xposition.Row - 1), j = (byte)(board.Xposition.Col - 1); i < 8 && j < 8; i--, j--)
            {
                if (board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, j));
                }
                else { break; }
            }

            #endregion

            return moves;
        }
    }
}
