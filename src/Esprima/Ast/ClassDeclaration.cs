using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class ClassDeclaration : Declaration, IClass
    {
        public readonly Expression? Id; // Identifier || StaticIdentifier
        Expression? IClass.Id => Id;

        public readonly Expression? SuperClass; // Identifier || CallExpression
        Expression? IClass.SuperClass => SuperClass;

        public readonly Statement Body;
        Statement IClass.Body => Body;

        public bool IsModule { get; set; }

        public ClassDeclaration(Expression? id, Expression? superClass, Statement body) :
            base(Nodes.ClassDeclaration)
        {
            Id = id;
            SuperClass = superClass;
            Body = body;
        }

        public override NodeCollection ChildNodes => new(Id, SuperClass, Body);

        protected internal override void Accept(AstVisitor visitor)
        {
            visitor.VisitClassDeclaration(this);
        }
    }
}
