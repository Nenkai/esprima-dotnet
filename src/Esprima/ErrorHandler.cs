﻿namespace Esprima
{
    /// <summary>
    /// Default error handling logic for Esprima.
    /// </summary>
    public class ErrorHandler : IErrorHandler
    {
        public string? Source { get; set; }
        public bool Tolerant { get; set; }

        public virtual void RecordError(ParserException error)
        {
        }

        public void Tolerate(ParserException error)
        {
            if (Tolerant)
            {
                RecordError(error);
            }
            else
            {
                throw error;
            }
        }

        public ParserException CreateError(int startIndex, int endIndex, int line, int col, string description)
        {
            return new ParserException(new ParseError(description, Source, startIndex, endIndex, new Position(line, col)));
        }

        public void TolerateError(int startIndex, int endIndex, int line, int col, string description)
        {
            var error = CreateError(startIndex, endIndex, line, col, description);
            if (Tolerant)
            {
                RecordError(error);
            }
            else
            {
                throw error;
            }
        }
    }
}
