using static Esprima.EsprimaExceptionHelper;

namespace Esprima
{
    public class ParseError
    {
        public string Description { get; }
        public string? Source { get; }

        public bool IsIndexDefined => StartIndex >= 0;
        public int StartIndex { get; }
        public int EndIndex { get; }

        public bool IsPositionDefined => Position.Line > 0;
        public Position Position { get; }
        public int LineNumber => Position.Line;
        public int Column => Position.Column;

        public ParseError(string description) :
            this(description, null, -1, -1, default)
        {
        }

        public ParseError(string description,
            string? source, int startIndex, int endIndex, Position position)
        {
            Description = description ?? ThrowArgumentNullException<string>(nameof(description));
            Source = source;
            StartIndex = startIndex;
            EndIndex = endIndex;
            Position = position;
        }

        public override string ToString()
        {
            return LineNumber > 0 ? $"Line {LineNumber}: {Description}" : Description;
        }
    }
}
