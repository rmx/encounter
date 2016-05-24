module rmx.Storage {

    export class Image {
        blobId    : string;
        relatedTo : string;
    }

    Avers.definePrimitive (Image, 'blobId');
    Avers.definePrimitive (Image, 'relatedTo');
}
