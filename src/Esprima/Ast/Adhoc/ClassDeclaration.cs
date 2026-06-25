using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Id), nameof(SuperClass), nameof(Body) })]
public sealed partial class ClassDeclaration : Declaration, IClass
{
    public ClassDeclaration(Expression? id, Expression? superClass, Statement body) :
        base(Nodes.ClassDeclaration)
    {
        Id = id;
        SuperClass = superClass;
        Body = body;
    }

    public Expression? Id { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public Expression? SuperClass { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public Statement Body { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ClassDeclaration Rewrite(Expression? id, Expression? superClass, Statement body)
    {
        return new ClassDeclaration(id, superClass, body);
    }
}
