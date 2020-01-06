class Tree
	attr_accessor :children, :node_name

	def initialize(root)
		if root.class != {}.class
			raise "different type"
		elsif root.length != 1
			raise "size error"
		end
		@node_name = root.keys[0]
		@children = []
		root[@node_name].each do |name, childs|
			node = Tree.new({name=>childs})
			@children.push(node)
		end
	end

	def visit_all(&block)
		visit &block
		children.each {|c| c.visit_all &block}
	end

	def visit(&block)
		block.call self
	end
end


d = {"grandpa" => {
	'dad'=>{'child1'=>{}, 'child2'=>{} }, 
	'uncle'=>{'child3'=>{}, 'child4'=>{} }
}}

node = Tree.new(d)
node.

node.visit_all {|node| puts node.node_name}



