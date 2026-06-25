namespace Esprima.Ast;

/// <summary>
/// Represents either a <see cref="ClassDeclaration"/> or an <see cref="ClassExpression"/>
/// </summary>
public interface IClass
{
    Nodes Type { get; }
    Expression? Id { get; }
    Expression? SuperClass { get; }
    Statement Body { get; }
    ChildNodes ChildNodes { get; }
}
