using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode]
public sealed partial class SelfExpression : Expression
{
    public SelfExpression() : base(Nodes.SelfExpression)
    {
    }
}
