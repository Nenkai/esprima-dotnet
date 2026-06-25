using System;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode]
public sealed partial class PragmaNoStrictStatement : Statement
{
    public PragmaNoStrictStatement() : base(Nodes.PragmaNoStrictStatement)
    {

    }
}
