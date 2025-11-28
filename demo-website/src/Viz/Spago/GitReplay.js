// Git replay module - animates through commit history
// Shows which modules were created/modified over time

let timelineData = null;
let replayState = {
  isPlaying: false,
  currentIndex: 0,
  speed: 100, // ms per commit
  intervalId: null,
  fadeSteps: 5, // How many commits before red fades to green
  moduleStates: new Map(), // moduleName -> { exists: bool, heat: 0-1 }
  onUpdate: null // Callback when state changes
};

// Load timeline data
export function loadTimeline_() {
  fetch('./data/commit-timeline.json')
    .then(response => response.json())
    .then(data => {
      timelineData = data;
      // Initialize all modules as non-existent
      replayState.moduleStates.clear();
      console.log('Loaded commit timeline:', data.commitCount, 'commits,', data.moduleCount, 'modules');
    })
    .catch(err => {
      console.warn('Failed to load commit timeline:', err);
    });
}

// Check if timeline is loaded
export function timelineLoaded_() {
  return timelineData !== null;
}

// Get total number of commits
export function getCommitCount_() {
  return timelineData ? timelineData.commitCount : 0;
}

// Get current commit index
export function getCurrentIndex_() {
  return replayState.currentIndex;
}

// Get current commit info
export function getCurrentCommit_() {
  if (!timelineData || replayState.currentIndex >= timelineData.timeline.length) {
    return null;
  }
  return timelineData.timeline[replayState.currentIndex];
}

// Set callback for state updates
export function setUpdateCallback_(callback) {
  return function() {
    replayState.onUpdate = callback;
  };
}

// Get module color state for current replay position
// Returns: "none" (doesn't exist), "created" (yellow), "hot" (red), "warm" (orange), "cool" (green)
export function getModuleReplayState_(moduleName) {
  const state = replayState.moduleStates.get(moduleName);
  if (!state || !state.exists) {
    return "none";
  }
  if (state.heat >= 1.0) {
    return "created";
  }
  if (state.heat > 0.6) {
    return "hot";
  }
  if (state.heat > 0.3) {
    return "warm";
  }
  return "cool";
}

// Get heat value (0-1) for a module
export function getModuleHeat_(moduleName) {
  const state = replayState.moduleStates.get(moduleName);
  if (!state || !state.exists) {
    return 0;
  }
  return state.heat;
}

// Step to next commit
function stepForward() {
  if (!timelineData || replayState.currentIndex >= timelineData.timeline.length) {
    stopReplay();
    return false;
  }

  const commit = timelineData.timeline[replayState.currentIndex];

  // Cool down all existing modules
  for (const [name, state] of replayState.moduleStates) {
    if (state.exists && state.heat > 0) {
      state.heat = Math.max(0, state.heat - (1 / replayState.fadeSteps));
    }
  }

  // Process created modules
  for (const moduleName of commit.created) {
    replayState.moduleStates.set(moduleName, { exists: true, heat: 1.0 });
  }

  // Process modified modules
  for (const moduleName of commit.modified) {
    const state = replayState.moduleStates.get(moduleName);
    if (state) {
      state.heat = 0.9; // Slightly less than created
    } else {
      // Module exists but we hadn't tracked it yet
      replayState.moduleStates.set(moduleName, { exists: true, heat: 0.9 });
    }
  }

  // Process deleted modules
  for (const moduleName of commit.deleted) {
    const state = replayState.moduleStates.get(moduleName);
    if (state) {
      state.exists = false;
      state.heat = 0;
    }
  }

  replayState.currentIndex++;

  // Notify callback
  if (replayState.onUpdate) {
    replayState.onUpdate();
  }

  return true;
}

// Start replay
export function startReplay_() {
  if (replayState.isPlaying) return;

  replayState.isPlaying = true;
  replayState.intervalId = setInterval(() => {
    if (!stepForward()) {
      stopReplay();
    }
  }, replayState.speed);
}

// Stop/pause replay
export function stopReplay_() {
  stopReplay();
}

function stopReplay() {
  replayState.isPlaying = false;
  if (replayState.intervalId) {
    clearInterval(replayState.intervalId);
    replayState.intervalId = null;
  }
}

// Reset to beginning
export function resetReplay_() {
  stopReplay();
  replayState.currentIndex = 0;
  replayState.moduleStates.clear();
  if (replayState.onUpdate) {
    replayState.onUpdate();
  }
}

// Set speed (ms per commit)
export function setReplaySpeed_(speed) {
  return function() {
    replayState.speed = speed;
    // If playing, restart with new speed
    if (replayState.isPlaying) {
      stopReplay();
      startReplay_();
    }
  };
}

// Set fade steps (how many commits to fade from red to green)
export function setFadeSteps_(steps) {
  return function() {
    replayState.fadeSteps = steps;
  };
}

// Check if playing
export function isPlaying_() {
  return replayState.isPlaying;
}

// Jump to specific commit index
export function jumpToCommit_(index) {
  return function() {
    if (!timelineData) return;

    // Reset and replay up to the target index
    replayState.moduleStates.clear();
    replayState.currentIndex = 0;

    // Fast-forward through commits
    while (replayState.currentIndex < index && replayState.currentIndex < timelineData.timeline.length) {
      const commit = timelineData.timeline[replayState.currentIndex];

      // Cool down all modules
      for (const [name, state] of replayState.moduleStates) {
        if (state.exists && state.heat > 0) {
          state.heat = Math.max(0, state.heat - (1 / replayState.fadeSteps));
        }
      }

      // Process this commit
      for (const moduleName of commit.created) {
        replayState.moduleStates.set(moduleName, { exists: true, heat: 1.0 });
      }
      for (const moduleName of commit.modified) {
        const state = replayState.moduleStates.get(moduleName);
        if (state) {
          state.heat = 0.9;
        } else {
          replayState.moduleStates.set(moduleName, { exists: true, heat: 0.9 });
        }
      }
      for (const moduleName of commit.deleted) {
        const state = replayState.moduleStates.get(moduleName);
        if (state) {
          state.exists = false;
          state.heat = 0;
        }
      }

      replayState.currentIndex++;
    }

    if (replayState.onUpdate) {
      replayState.onUpdate();
    }
  };
}

// Get color for replay state
export function getReplayColor_(moduleName) {
  const state = replayState.moduleStates.get(moduleName);

  if (!state || !state.exists) {
    return "rgba(128, 128, 128, 0.1)"; // Barely visible gray
  }

  const heat = state.heat;

  if (heat >= 1.0) {
    // Created - bright yellow
    return "rgb(255, 235, 59)";
  } else if (heat > 0) {
    // Interpolate from red (hot) to green (cool)
    // red (255, 82, 82) -> orange (255, 167, 38) -> green (76, 175, 80)
    if (heat > 0.5) {
      // Red to orange
      const t = (heat - 0.5) * 2;
      const r = 255;
      const g = Math.round(82 + (167 - 82) * (1 - t));
      const b = Math.round(82 + (38 - 82) * (1 - t));
      return `rgb(${r}, ${g}, ${b})`;
    } else {
      // Orange to green
      const t = heat * 2;
      const r = Math.round(76 + (255 - 76) * t);
      const g = Math.round(175 + (167 - 175) * t);
      const b = Math.round(80 + (38 - 80) * t);
      return `rgb(${r}, ${g}, ${b})`;
    }
  } else {
    // Cool - green
    return "rgb(76, 175, 80)";
  }
}
