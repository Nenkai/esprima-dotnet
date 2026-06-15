using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class PragmaPushStrictStatement : Statement
    {
        public Literal Value { get; set; } // Literal (Bool or Int)

        public PragmaPushStrictStatement(Literal value) : base(Nodes.PragmaPushStrictStatement)
        {
            Value = value;
        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
