using System;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode]
public sealed partial class PragmaPopStrictStatement : Statement
{
    public PragmaPopStrictStatement() : base(Nodes.PragmaPopStrictStatement)
    {

    }
}
