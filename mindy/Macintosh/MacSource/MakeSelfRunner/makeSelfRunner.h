// makeSelfRunner.h

OSErr MakeSelfRunner( FSSpec * file, int creator, int type, short sizeFlags, int minimumPartition, int preferredPartition );
OSErr AddRunnerResources( FSSpec * file, int creator, short type );
OSErr SetFinderInfo( FSSpec * file, int creator, int type );
OSErr SetSizeResource( FSSpec * file, UInt16 flags, UInt32 minimumSize, UInt32 preferredSize );