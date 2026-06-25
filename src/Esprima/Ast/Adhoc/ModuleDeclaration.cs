using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Id), nameof(Body) })]
public sealed partial class ModuleDeclaration : Declaration
{
    public ModuleDeclaration(Expression? id, Statement body) :
        base(Nodes.ModuleDeclaration)
    {
        Id = id;
        Body = body;
    }

    public Expression? Id { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; } // Identifier || StaticIdentifier
    public Statement Body { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ModuleDeclaration Rewrite(Expression? id, Statement body)
    {
        return new ModuleDeclaration(id, body);
    }
}
