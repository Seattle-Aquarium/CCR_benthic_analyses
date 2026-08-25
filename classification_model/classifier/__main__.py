"""``python -m classifier`` opens the GUI; ``python -m classifier.cli`` is headless."""
from .gui.app import main

if __name__ == "__main__":
    main()
