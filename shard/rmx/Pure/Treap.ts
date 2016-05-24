export class TreapNode {

    key      : number;
    priority : number;
    data     : any;

    left_child  : any;
    right_child : any;
    private parent      : any;


    constructor(key: number, priority: number, data: any) {
        this.key = key;
        this.priority = priority;
        this.data = data;

        this.left_child  = null;
        this.right_child = null;
        this.parent      = null;
    }

    hasKey(key: number): boolean {
        if(this.key === key)
            return true;
        if(this.key < key && this.left_child != null) {
            return this.left_child.hasKey(key);
        } else if(this.key > key && this.right_child != null) {
            return this.right_child.hasKey(key);
        } else {
            return false;
        }
    }

    getRoot(): TreapNode {
        if(this.parent === null)
            return this;
        return this.parent.getRoot();
    }


    // First we insert the node in a binary search tree manner and in a
    // subsequent step ensure that the heap property still holds
    insert(node: TreapNode): TreapNode {
        if(this.key === node.key)
            throw new Error("Key #{node.key} already exists!");

        var new_root = null;

        if(node.key < this.key) {
            if(this.left_child == null) {
                node.parent = this;
                this.left_child = node;
            } else
                this.left_child = this.left_child.insert(node);

            new_root = this;
            if(this.left_child.priority < this.priority)
                new_root = this.rotateRight();
        } else {
            if(this.right_child == null) {
                node.parent = this;
                this.right_child = node;
            } else
                this.right_child = this.right_child.insert(node);

            new_root = this;
            if(this.right_child.priority < this.priority)
                new_root = this.rotateLeft();
        }

        return new_root;
    }


    // Locate and remove a key from the treap. This is used as well upon
    // extracting the minimum.
    remove(key: number): TreapNode {

        var new_root = this;

        if(this.key === key)
            new_root = this.removeNode();

        if(key < this.key) {
            if(this.left_child != null)
                this.left_child = this.left_child.remove(key);
        } else {
            if(this.right_child != null)
                this.right_child = this.right_child.remove(key);
        }

        return new_root;
    }


    // Rotate the node all the way down and then delete it (base cases)
    private removeNode(): TreapNode {

        if(this.left_child == null)
            return this.right_child;

        if(this.right_child == null)
            return this.left_child;

        if ((this.right_child == null) && (this.left_child == null))
            return null;

        var res;
        if(this.left_child.priority < this.right_child.priority) {
            res = this.rotateRight();
            res.right_child = res.right_child.removeNode();
        } else {
            res = this.rotateLeft();
            res.left_child = res.left_child.removeNode();
        }

        return res;
    }


    rotateLeft(): TreapNode {

        var new_root = this.right_child;

        this.assignParentsChild(new_root);
        new_root.parent = this.parent;
        this.parent = new_root;

        this.right_child = new_root.left_child;
        if(new_root.left_child != null)
            new_root.left_child.parent = this;

        new_root.left_child = this;

        return new_root;
    }


    rotateRight(): TreapNode {

        var new_root = this.left_child;

        this.assignParentsChild(new_root);
        new_root.parent = this.parent;
        this.parent = new_root;

        this.left_child = new_root.right_child;
        if(new_root.right_child != null)
            new_root.right_child.parent = this;

        new_root.right_child = this;

        return new_root;
    }


    // Traverse the treap in-order
    visit(process_node): void {
        if(this.left_child != null)
            this.left_child.visit(process_node);
        if(this.key != null)
            process_node(this.key);
        if(this.right_child != null)
            this.right_child.visit(process_node);
    }


    // Update the parent of the old root node (at rotation)
    private assignParentsChild(new_child: TreapNode): void {
        if(this.parent != null && this.right_child != null &&
           this.right_child.key === this.key) {
            this.parent.right_child = new_child;
        } else if(this.parent != null && this.left_child != null &&
           this.left_child.key === this.key) {
            this.parent.left_child = new_child;
        }
    }

}


export class Treap {

    private root: TreapNode;

    constructor() {
        this.root = null;
    }

    hasKey(key: number): boolean {
        if(this.root != null) {
            return this.root.hasKey(key);
        } else {
            return false;
        }
    }

    isEmpty(): boolean {
        return this.root == null;
    }

    size(): number {
        var count = 0;
        this.visit(function() {
            return count += 1;
        });
        return count;
    }

    visit(action): void {
        if(this.root != null)
            this.root.visit(action);
    }

    insert(key: number, priority: number, data: any): void {
        var node = new TreapNode(key, priority, data);
        if(this.root != null) {
            this.root = this.root.insert(node);
        } else {
            this.root = node;
        }
    }

    insertNode(node: TreapNode): void {
        if(this.root != null) {
            this.root = this.root.insert(node);
        } else {
            this.root = node;
        }
    }

    remove(key: number): void {
        if(this.root != null)
            this.root = this.root.remove(key);
    }

    clear(): void {
        this.root = null;
    }

    extractMin(): TreapNode {
        if(this.root != null) {
            var head = this.root;
            this.root = this.root.remove(this.root.key);
            return head;
        }
        return null;
    }

    peek(): TreapNode {
        if(this.root != null) {
            return this.root;
            //return {key: this.root.key, priority: this.root.priority,
                    //data: this.root.data};
        } else
            return null;
    }
}
