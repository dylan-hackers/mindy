module: dylan-user

define library threads
  use dylan;
  export threads;
end library threads;

define module threads
  use dylan;
  
  export dynamic-bind,
    <synchronization>, <exclusive-lock>,
    <semaphore>, <recursive-lock>,
    <read-write-lock>,
    <lock>, <simple-lock>, with-lock,
    <thread>, atomic-increment!, current-thread,
    <notification>, wait-for, release-all;
end module threads;
