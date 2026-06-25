using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode]
public sealed partial class ErrorExpression : Expression
{
    public ErrorExpression() : base(Nodes.ExpressionStatement)
    {

    }
}
