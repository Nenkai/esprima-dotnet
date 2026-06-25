using System.Runtime.CompilerServices;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Decorators), nameof(Id), nameof(SuperClass), nameof(Body) })]
public sealed partial class ClassExpression : Expression, IClass
{
    private readonly NodeList<Decorator> _decorators;

    public ClassExpression(
        Expression? id,
        Expression? superClass,
        Statement body,
        in NodeList<Decorator> decorators) : base(Nodes.ClassExpression)
    {
        Id = id;
        SuperClass = superClass;
        Body = body;
        _decorators = decorators;
    }

    public Expression? Id { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public Expression? SuperClass { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public Statement Body { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public ref readonly NodeList<Decorator> Decorators { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => ref _decorators; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ClassExpression Rewrite(in NodeList<Decorator> decorators, Expression? id, Expression? superClass, Statement body)
    {
        return new ClassExpression(id, superClass, body, decorators);
    }
}
