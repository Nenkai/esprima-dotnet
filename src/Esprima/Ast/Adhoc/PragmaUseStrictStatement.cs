using System;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode]
public sealed partial class PragmaUseStrictStatement : Statement
{
    public PragmaUseStrictStatement() : base(Nodes.PragmaUseStrictStatement)
    {

    }
}
