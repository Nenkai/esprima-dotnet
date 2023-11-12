using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class ClassExpression : Expression, IClass
    {
        public readonly Expression? Id;
        Expression? IClass.Id => Id;

        public readonly Expression? SuperClass;
        Expression? IClass.SuperClass => SuperClass;

        public readonly Statement Body;
        Statement IClass.Body => Body;

        public bool IsModule { get; set; }

        public ClassExpression(
            Expression? id,
            Expression? superClass,
            Statement body) : base(Nodes.ClassExpression)
        {
            Id = id;
            SuperClass = superClass;
            Body = body;
        }

        public override NodeCollection ChildNodes => new(Id, SuperClass/*, Body*/);

        protected internal override void Accept(AstVisitor visitor)
        {
            visitor.VisitClassExpression(this);
        }
    }
}
