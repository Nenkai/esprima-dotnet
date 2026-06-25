using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode]
public sealed partial class ErrorStatement : Statement
{
    public ErrorStatement() : base(Nodes.ErrorStatement)
    {

    }
}
