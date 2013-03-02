using System;

namespace FifteenPuzzle
{
    public static class MathUtil
    {
        public static int ManhattanDistance(byte fromRow, byte fromCol, byte toRow, byte toCol)
        {
            return Math.Abs(fromRow - toRow) + Math.Abs(fromCol - toCol);
        }
    }
}