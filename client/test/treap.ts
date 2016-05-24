/// <reference path="./node.d.ts" />
/// <reference path="../app/rmx/Pure/Treap.ts" />

var assert = require('assert');
var treap  = new rmx.Pure.Treap();

module.exports = {
  'treap operations': {
    'treap.isEmpty on empty works': function() {
      assert.ok(treap.isEmpty());
    },
    'treap.size on empty works': function() {
      assert.ok(treap.size() == 0);
    },
    'treap.size works': function() {
      treap.clear();
      assert.ok(treap.isEmpty());
      treap.insert(1, -1, "data");
      treap.insert(12, 1, "data");
      assert.ok(treap.size() == 2);
      treap.clear();
    },
    'treap.insert works': function() {
      treap.clear();
      assert.ok(treap.isEmpty());

      treap.insert(11, 0, "data");
      assert.ok(!treap.isEmpty());
      assert.ok(treap.hasKey(11));
      assert.ok(treap.size() == 1);

      var peeked_head = <any> treap.peek();
      assert.ok(peeked_head.key ==  11);
      assert.ok(peeked_head.priority == 0);
      assert.ok(peeked_head.data == "data");
      treap.clear();
    },
    'treap.extraction works': function() {
      treap.clear();
      assert.ok(treap.isEmpty());
      treap.insert(1, 0, {});
      treap.insert(121, -1, "data");

      assert.ok(treap.extractMin().data == "data");
      assert.ok(treap.size() == 1);
      treap.clear();
    },
    'treap.rotationLeft works': function() {
      treap.clear();
      assert.ok(treap.isEmpty());
      treap.insert(11, 1, {});
      treap.insert(22, 0, {});
      treap.insert(44, 2, {});
      treap.insert(33, 4, {});
      treap.insert(55, 5, {});

      assert.ok(treap.size() == 5);
      var new_root = (<any>treap.peek()).rotateLeft();
      assert.ok(new_root.key == 44);
      assert.ok(new_root.left_child.key == 22);
      assert.ok(new_root.left_child.right_child.key == 33);
      assert.ok(new_root.left_child.left_child.key == 11);
      assert.ok(new_root.right_child.key == 55);

      treap.clear();
    },
    'treap.rotationRight works': function() {
      treap.clear();
      assert.ok(treap.isEmpty());
      treap.insert(11, 3, {});
      treap.insert(22, 2, {});
      treap.insert(33, 4, {});
      treap.insert(44, 0, {});
      treap.insert(55, 5, {});

      assert.ok(treap.size() == 5);
      var new_root = (<any>treap.peek()).rotateRight();
      assert.ok(new_root.key == 22);
      assert.ok(new_root.left_child.key == 11);
      assert.ok(new_root.right_child.key == 44);
      assert.ok(new_root.right_child.right_child.key == 55);
      assert.ok(new_root.right_child.left_child.key == 33);

      treap.clear();
    },
    'treap.remove works': function() {
      treap.clear();
      assert.ok(treap.isEmpty());
      treap.insert(11, 1, {});
      treap.insert(22, 0, {});
      treap.insert(44, 2, {});
      treap.insert(33, 4, {});
      treap.insert(55, 5, {});

      console.log('====');
      assert.ok(treap.size() == 5);
      console.log('====');
      treap.remove(44);
      console.log(treap.size());
      console.log('====');
      assert.ok(treap.size() == 4);

      var root = <any> treap.peek();
      assert.ok(root.key == 22);
      assert.ok(root.left_child.key == 11);
      assert.ok(root.right_child.key == 33);
      assert.ok(root.right_child.right_child.key == 55);

      treap.clear();
    },
    'treap.permute insert works': function() {
      treap.clear();
      treap.insert(77, 10, ['z']);
      treap.insert(66, 1,  ['x']);
      treap.insert(55, 100,['u']);
      treap.insert(44, -1, ['t']);
      treap.insert(11, 21, ['a']);
      treap.insert(33, 11, ['s']);
      treap.insert(22, 5,  ['e']);
      assert.ok(treap.size() == 7);

      var peak = treap.extractMin();
      assert.ok(treap.size() == 6);
      assert.ok(peak.data[0] == "t");
      peak = treap.extractMin();
      assert.ok(treap.size() == 5);
      assert.ok(peak.data[0] == "x");
      peak = treap.extractMin();
      assert.ok(treap.size() == 4);
      assert.ok(peak.data[0] == "e");
      peak = treap.extractMin();
      assert.ok(treap.size() == 3);
      assert.ok(peak.data[0] == "z");
      peak = treap.extractMin();
      assert.ok(treap.size() == 2);
      assert.ok(peak.data[0] == "s");
      peak = treap.extractMin();
      assert.ok(treap.size() == 1);
      assert.ok(peak.data[0] == "a");
      peak = treap.extractMin();
      assert.ok(treap.size() == 0);
      assert.ok(peak.data[0] == "u");
    },
  }
};
