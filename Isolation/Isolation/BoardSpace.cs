using System;

namespace Isolation
{
    public class BoardSpace : IEquatable<BoardSpace>
    {
        public byte Row { get; private set; }
        public byte Col { get; private set; }

        public BoardSpace(byte row, byte col)
        {
            Row = row;
            Col = col;
        }

        public BoardSpace(string fromString)
        {
            try
            {
                var parts = fromString.TrimStart('(').TrimEnd(')').Split(' ');

                // board spaces are stored 0-7, displayed 1-8, so subtract 1
                Row = (byte)(byte.Parse(parts[0]) - 1);
                Col = (byte)(byte.Parse(parts[1]) - 1);
            }
            catch
            {
                throw new ArgumentException("Invalid board space string.");
            }
        }

        public bool Equals(BoardSpace other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return Row == other.Row && Col == other.Col;
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as BoardSpace);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (Row.GetHashCode() * 397) ^ Col.GetHashCode();
            }
        }

        public override string ToString()
        {
            // board spaces are stored 0-7, displayed 1-8, so add 1
            return string.Format("({0} {1})", Row + 1, Col + 1);
        }
    }
}