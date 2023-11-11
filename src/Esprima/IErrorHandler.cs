namespace Esprima
{
    public interface IErrorHandler
    {
        string? Source { get; set; }
        bool Tolerant { get; set; }
        void RecordError(ParserException error);
        void Tolerate(ParserException error);
        ParserException CreateError(int startIndex, int endIndex, int line, int column, string message);
        void TolerateError(int startIndex, int endIndex, int line, int column, string message);
    }
}
