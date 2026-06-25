using Esprima.Ast.Adhoc;

namespace Esprima.Ast;

/// <summary>
/// Represents either a <see cref="FunctionDeclaration"/> or a <see cref="MethodExpression"/>
/// </summary>
public interface IFunction
{
    Nodes Type { get; }
    Identifier? Id { get; }
    ref readonly NodeList<Expression> Params { get; }
    BlockStatement Body { get; }
    bool Generator { get; }
    bool Expression { get; }
    bool Strict { get; }
    bool Async { get; }
    ChildNodes ChildNodes { get; }
}
