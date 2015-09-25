SET(inspector-base_SOURCES
    inspector-base.dylan
)
ADD_DYLAN_LIBRARY(inspector-base)

SET(text-inspector_SOURCES
    text-inspector.dylan
)
ADD_DYLAN_LIBRARY(text-inspector)

SET(x-inspector_SOURCES
    x-library.dylan
    class-diagram.dylan
    x-inspector.dylan
)
ADD_DYLAN_LIBRARY(x-inspector)
