import { Vec3, vec3 } from 'gl-matrix';

import { getWorldCoordinates, terrainPositionAt, Terrain, TerrainPosition } from '../Pure/Terrain';
import { HalfEdgeEdge, HalfEdgeFace } from './Navmesh';


// toNavmeshCoordinates
// -----------------------------------------------------------------------
//
// Map the given world coordinates to a position on the navmesh.

function
toNavmeshCoordinates(terrain: Terrain, x: Vec3): Vec3 {
    var p = terrainPositionAt(terrain, x);
    if (p) {
        return getWorldCoordinates(terrain, p);
    }
}


function vec3equal(a: Vec3, b: Vec3): boolean {
    return vec3.distance(a, b) < 0.0001;
}

export class PathFinder {

    // sets (FIXME use Treap) holding visited and evaluated navigation mesh
    // faces
    private _open_list   : { [id: number]: HalfEdgeFace };
    private _closed_list : { [id: number]: HalfEdgeFace };

    private _path_length : { [id: number]: number };
    private _cost_map    : { [id: number]: number };

    private _current_pos : Vec3;


    constructor
      ( public terrain    : Terrain
        // ^ MUST be the terrain where 'start_pos' is located in.

      , public from       : HalfEdgeFace
      , public to         : HalfEdgeFace
      , public start_pos  : TerrainPosition
      , public target_pos : TerrainPosition
      ) {
        this._open_list   = {};
        this._closed_list = {};
        this._path_length = {};
        this._cost_map    = {};

        this._current_pos = vec3.clone(getWorldCoordinates(this.terrain, this.start_pos));
    }


    compute(): Vec3[] {
        var path = [];

        this._path_length[this.from.id] = 0;
        this._cost_map[this.from.id]    = this.estimateCostToDestination(this.from);
        this._open_list[this.from.id]   = this.from;

        while (Object.keys(this._open_list).length > 0) {
            var best = this.mostPromisingFace();

            // finish and smooth path if we arrived at destination
            if (best.id === this.to.id)
                return this.nextCornerPathReconstruction(this.to, path);


            this._closed_list[best.id] = best;
            delete this._open_list[best.id];
            best.forEachNeighborFace(neighbor => {
                if (this._closed_list[neighbor.id] == null) {
                    //FIXME: the distance or cost between best and
                    //       neighbor should be determined by the terrain
                    //       (here we use Manhattan distance to simplify
                    //       things)
                    var new_path_length = this._path_length[best.id] +
                                          this.manhattenDist(best, neighbor);
                    // add or update neighbor path length
                    if ((this._open_list[neighbor.id] == null) ||
                         new_path_length < this._path_length[neighbor.id]) {

                        this._open_list[neighbor.id]   = neighbor;
                        this._path_length[neighbor.id] = new_path_length;
                        this._cost_map[neighbor.id]    = this._cost_through(neighbor);

                        // coming from best when backtracking path
                        path[neighbor.id] = best;
                    }
                }
            });
        }
    }

    // Find the best tile in the open list
    //FIXME: use treap.extractMin()
    private mostPromisingFace(): HalfEdgeFace {
        var best, best_id, id, item, _ref;
        best_id = Object.keys(this._open_list)[0];
        best = this._open_list[best_id];
        _ref = this._open_list;
        for (id in _ref) {
            item = _ref[id];
            if (this._cost_map[id] < this._cost_map[best.id]) {
                best = item;
            }
        }
        return best;
    }

    // Reconstructs the path and returns all tiles on path.
    private reconstructPath(current: HalfEdgeFace,
                            path: HalfEdgeFace[]): HalfEdgeFace[] {
        if (current == null) {
            return [];
        } else {
            var p = this.reconstructPath(path[current.id], path);
            return p.concat(current);
        }
    }

    // Reconstructs the path and returns waypoints connecting start and
    // target position with corners.
    private nextCornerPathReconstruction(current: HalfEdgeFace,
            path: HalfEdgeFace[]): Vec3[] {

        var nav_mesh_path  = this.reconstructPath(current, path)
        , reset          = true
        , best_left      = null
        , best_right     = null
        , best_right_dir = vec3.create()
        , best_left_dir  = vec3.create()
        , corner_path    = [];

        corner_path.push(this._current_pos);

        //while we have a next nav mesh face on the path we:
        //  - compute new line of sight, left & right corner and direction
        //  - check if we still "see" the new left & right corners
        //  - update best values or add corner "blocking" the line of sight
        //    to waypoint list
        while (nav_mesh_path.length > 1) {
            var current_face = nav_mesh_path.shift()
              , sharing_edge = this.getSharingEdge(current_face, nav_mesh_path[0])
              , right        = vec3.clone(sharing_edge.begin.pos)
              , left         = vec3.clone(sharing_edge.end.pos)
              , right_dir    = vec3.create()
              , left_dir     = vec3.create();

            vec3.subtract(right_dir, right, this._current_pos);
            vec3.subtract(left_dir, left, this._current_pos);

            if (reset === true) {
                best_left      = left;
                best_right     = right;
                best_left_dir  = left_dir;
                best_right_dir = right_dir;
                reset          = false;
                continue;
            }

            // The new corners are acceptable if the corridor gets narrower
            // or stays the same. In order to choose the correct corner
            // later (in case 3) we need to keep track which was updated.
            //FIXME: the narrowing should only be done if we hit a wall!
            var right_changed = false
              , left_changed  = false;

            if (this._isLeftEq(best_right_dir, right_dir)) {
                right_changed  = !vec3equal(best_right, right);
                best_right     = right;
                best_right_dir = right_dir;
            }

            if (this._isRightEq(best_left_dir, left_dir)) {
                left_changed  = !vec3equal(best_left, left);
                best_left     = left;
                best_left_dir = left_dir;
            }

            // Case 1: the new right direction is right of the best right
            // direction. This means the right corner blocks our view to
            // the next nav mesh face on the path.
            if (this._isRight(best_right_dir, right_dir)) {
                reset = true;
                this._current_pos = toNavmeshCoordinates(this.terrain, best_right);
                corner_path.push(this._current_pos);
            } else if (this._isLeft(best_left_dir, left_dir)) {
                // Case 2: same as case 1 for left direction.
                reset = true;
                this._current_pos = toNavmeshCoordinates(this.terrain, best_left);
                corner_path.push(this._current_pos);
            } else if (this._isRightEq(best_right_dir, best_left_dir)) {
                // Case 3: the best left direction is now right of the
                // best right direction (same as best left is left of best
                // right). To find the proper corner blocking the line of
                // sight we need to know which corner was updated in this
                // step.
                reset = true;

                var pos;
                if (right_changed) {
                    pos = toNavmeshCoordinates(this.terrain, best_left);
                } else {
                    pos = toNavmeshCoordinates(this.terrain, best_right);
                }

                this._current_pos = pos;
                corner_path.push(this._current_pos);
            }
        }

        // handle last nav face mesh (can be around the corner)
        if (!reset) {
            var target_dir = vec3.create();
            vec3.subtract(target_dir,
                    getWorldCoordinates(this.terrain, this.target_pos),
                    this._current_pos);
            if (this._isLeft(best_left_dir, target_dir)) {
                pos = toNavmeshCoordinates(this.terrain, best_left);
                corner_path.push(pos);
            } else if (this._isRight(best_right_dir, target_dir)) {
                pos = toNavmeshCoordinates(this.terrain, best_right);
                corner_path.push(pos);
            }
        }

        corner_path.push(
            vec3.clone(getWorldCoordinates(this.terrain, this.target_pos)));
        return corner_path;
    }


    // Check if the current direction is right of the ahead direction
    private _isRight(ahead, direction): boolean {
        return ahead[0] * direction[1] - ahead[1] * direction[0] < 0;
    }

    private _isRightEq(ahead, direction): boolean {
        return ahead[0] * direction[1] - ahead[1] * direction[0] <= 0;
    }

    // Check if the current direction is left of the ahead direction
    private _isLeft(ahead, direction): boolean {
        return ahead[0] * direction[1] - ahead[1] * direction[0] > 0;
    }

    private _isLeftEq(ahead, direction): boolean {
        return ahead[0] * direction[1] - ahead[1] * direction[0] >= 0;
    }


    // Get edge shared between face and neighbor
    //FIXME: orientation has changed (?)
    private getSharingEdge(face: HalfEdgeFace,
                           neighbor: HalfEdgeFace): HalfEdgeEdge {
        var edge_it, edge_it_end;
        edge_it_end = edge_it = face.edge;
        while (true) {
            if (edge_it.twin != null &&
                edge_it.twin.face === neighbor) {
                return edge_it;
            }
            edge_it = edge_it.next;
            if (edge_it === edge_it_end) {
                break;
            }
        }
    }

    // Computes the estimate cost of the path from the current tile to the
    // target.
    // Currently we use a Manhattan distance to approximate the cost to the
    // target. Later we may want to use precomputed path information
    // (editor, waypoint system?).
    private estimateCostToDestination(current: HalfEdgeFace): number {
        return this.manhattenDist(current, this.to);
    }

    // Computes the manhatten distance between two tiles
    private manhattenDist(from: HalfEdgeFace, to: HalfEdgeFace): number {
        var from_centroid, to_centroid;
        from_centroid = from.computeCentroid();
        to_centroid   = to.computeCentroid();
        return Math.abs(to_centroid[0] - from_centroid[0]) +
               Math.abs(to_centroid[1] - from_centroid[1]) +
               Math.abs(to_centroid[2] - from_centroid[2]);
    }

    // How much does it cost to go over this tile.
    private _cost_through(face: HalfEdgeFace): number {
        return this._path_length[face.id] +
               this.estimateCostToDestination(face);
    }
}
