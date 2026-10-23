"""
Where the Zooniverse password comes from, and where it must never appear.

Two things are being pinned here. One is the resolution order -- environment,
then the OS credential store, then the files -- and that `source` always names
the winner, because an order nobody can see is what makes any order dangerous.

The other is containment. A password passes through this module and must not
come out anywhere a person or a log file can read it: not in a repr, not in a
log line, not in the status string the GUI shows, and above all not in the
settings JSON, which lives in a folder OneDrive syncs.

The real credential store is never touched: `keyring` is replaced with an
in-memory stand-in for every test here.
"""

from __future__ import annotations

import logging
import sys
import types
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from kelpquest import zooniverse as z

USER = "m-williams"
#: Not a real password. Distinctive so a leak is unmistakable in any output.
SECRET = "correct-horse-battery-staple-7731"


class FakeKeyring:
    """An in-memory stand-in for the OS credential store."""

    def __init__(self, backend: str = "WinVaultKeyring") -> None:
        self.store: dict[tuple[str, str], str] = {}
        self._backend = type(backend, (), {})

    def get_keyring(self):
        return self._backend()

    def get_password(self, service, username):
        return self.store.get((service, username))

    def set_password(self, service, username, password):
        self.store[(service, username)] = password

    def delete_password(self, service, username):
        del self.store[(service, username)]        # KeyError when absent


@pytest.fixture
def vault(monkeypatch):
    """A fake credential store, with no credential files and a clean env."""
    fake = FakeKeyring()
    module = types.ModuleType("keyring")
    for name in ("get_keyring", "get_password", "set_password",
                 "delete_password"):
        setattr(module, name, getattr(fake, name))
    monkeypatch.setitem(sys.modules, "keyring", module)
    for key in (z.USER_KEY, z.PASS_KEY, z.PROJECT_KEY):
        monkeypatch.delenv(key, raising=False)
    monkeypatch.setattr(z, "ENV_CANDIDATES", ())
    monkeypatch.setattr(z, "_account", ("", ""))
    return fake


@pytest.fixture
def no_vault(monkeypatch):
    """A machine with no keyring installed at all."""
    monkeypatch.setitem(sys.modules, "keyring", None)
    for key in (z.USER_KEY, z.PASS_KEY, z.PROJECT_KEY):
        monkeypatch.delenv(key, raising=False)
    monkeypatch.setattr(z, "ENV_CANDIDATES", ())
    monkeypatch.setattr(z, "_account", ("", ""))


def _env_file(tmp_path: Path, **values) -> Path:
    path = tmp_path / "config.env"
    path.write_text("\n".join(f"{k}={v}" for k, v in values.items()) + "\n",
                    encoding="utf-8")
    return path


# --------------------------------------------------------------------------
#  containment
# --------------------------------------------------------------------------


def test_the_password_is_not_in_the_repr():
    """A dataclass prints every field, and this object reaches tracebacks."""
    creds = z.Credentials(username=USER, password=SECRET, project_id="24397")
    assert SECRET not in repr(creds)
    assert "password=set" in repr(creds)
    assert z.Credentials().__repr__().count("unset") == 1


def test_the_status_line_names_the_source_not_the_password(vault):
    vault.set_password(z.KEYRING_SERVICE, USER, SECRET)
    ready, message = z.credentials_status(USER, "24397")
    assert ready
    assert SECRET not in message
    assert USER in message and "24397" in message
    assert "credential store" in message


def test_saving_a_password_does_not_log_it(vault, caplog):
    with caplog.at_level(logging.DEBUG):
        z.save_credentials(USER, SECRET)
    logged = "\n".join(r.getMessage() for r in caplog.records)
    assert SECRET not in logged
    # Not decoration: without a positive assertion this test would pass on an
    # empty capture, and the module's logger does not propagate by default.
    assert USER in logged


def test_the_settings_file_has_nowhere_to_put_a_password(tmp_path: Path):
    """The one place a password must never be written. Checked structurally
    rather than by eye: a future field called `password` would fail this."""
    from kelpquest.config import AppConfig

    cfg = AppConfig()
    cfg.account.username = USER
    cfg.account.project_id = "24397"
    target = tmp_path / "settings.json"
    cfg.save(target)
    text = target.read_text(encoding="utf-8")
    assert USER in text
    assert "password" not in text.lower()
    assert SECRET not in text

    back = AppConfig.load(target)
    assert back.account.username == USER
    assert back.account.project_id == "24397"


# --------------------------------------------------------------------------
#  resolution order
# --------------------------------------------------------------------------


def test_nothing_anywhere_is_reported_as_not_signed_in(vault):
    ready, message = z.credentials_status()
    assert not ready
    assert "username" in message and "password" in message


def test_the_saved_password_is_used(vault):
    vault.set_password(z.KEYRING_SERVICE, USER, SECRET)
    creds = z.resolve(USER, "24397")
    assert creds.password == SECRET
    assert creds.source == "the OS credential store"
    assert creds.complete


def test_a_file_is_used_when_nothing_is_saved(vault, tmp_path, monkeypatch):
    path = _env_file(tmp_path, ZOONIVERSE_USERNAME=USER,
                     ZOONIVERSE_PASSWORD=SECRET,
                     ZOONIVERSE_PROJECT_ID="24397")
    monkeypatch.setattr(z, "ENV_CANDIDATES", (path,))
    creds = z.resolve()
    assert creds.password == SECRET
    assert creds.source == "config.env"
    assert creds.username == USER


def test_signing_in_wins_over_a_stale_file(vault, tmp_path, monkeypatch):
    """Otherwise signing in through the app appears to do nothing, because an
    old config.env quietly keeps supplying the password it replaced."""
    path = _env_file(tmp_path, ZOONIVERSE_USERNAME=USER,
                     ZOONIVERSE_PASSWORD="the-old-one",
                     ZOONIVERSE_PROJECT_ID="24397")
    monkeypatch.setattr(z, "ENV_CANDIDATES", (path,))
    vault.set_password(z.KEYRING_SERVICE, USER, SECRET)
    creds = z.resolve()
    assert creds.password == SECRET
    assert creds.source == "the OS credential store"


def test_the_environment_wins_over_everything(vault, tmp_path, monkeypatch):
    """Someone who exported it did so on purpose, probably for one run."""
    path = _env_file(tmp_path, ZOONIVERSE_PASSWORD="from-the-file")
    monkeypatch.setattr(z, "ENV_CANDIDATES", (path,))
    vault.set_password(z.KEYRING_SERVICE, USER, "from-the-store")
    monkeypatch.setenv(z.USER_KEY, USER)
    monkeypatch.setenv(z.PASS_KEY, SECRET)
    monkeypatch.setenv(z.PROJECT_KEY, "24397")
    creds = z.resolve()
    assert creds.password == SECRET
    assert creds.source == "the environment"


def test_reading_the_files_does_not_change_the_environment(vault, tmp_path,
                                                            monkeypatch):
    """This used to load them into os.environ, which made "came from the
    environment" and "came from a file" impossible to tell apart afterwards --
    and left the password in the environment of every subprocess."""
    import os

    path = _env_file(tmp_path, ZOONIVERSE_PASSWORD=SECRET)
    monkeypatch.setattr(z, "ENV_CANDIDATES", (path,))
    z.resolve()
    assert os.getenv(z.PASS_KEY) is None


def test_the_remembered_account_supplies_the_username(vault):
    """`export.inspect` calls connect() with no arguments, so the signed-in
    username has to be reachable without being passed down."""
    vault.set_password(z.KEYRING_SERVICE, USER, SECRET)
    z.set_account(USER, "24397")
    try:
        creds = z.resolve()
        assert creds.username == USER and creds.project_id == "24397"
        assert creds.password == SECRET
    finally:
        z.set_account("", "")


def test_a_username_with_no_saved_password_is_incomplete(vault):
    creds = z.resolve(USER, "24397")
    assert creds.username == USER
    assert not creds.password
    assert not creds.complete


# --------------------------------------------------------------------------
#  saving and forgetting
# --------------------------------------------------------------------------


def test_save_then_read_back(vault):
    z.save_credentials(USER, SECRET)
    assert vault.store[(z.KEYRING_SERVICE, USER)] == SECRET
    assert z.stored_password(USER) == SECRET


def test_forgetting_removes_it(vault):
    z.save_credentials(USER, SECRET)
    assert z.forget_credentials(USER) is True
    assert z.stored_password(USER) == ""
    # And saying so twice is not an error.
    assert z.forget_credentials(USER) is False


def test_each_username_has_its_own_password(vault):
    z.save_credentials(USER, SECRET)
    z.save_credentials("someone-else", "their-own")
    assert z.stored_password(USER) == SECRET
    assert z.stored_password("someone-else") == "their-own"


def test_without_a_keyring_nothing_is_written_anywhere(no_vault):
    """The fallback for a machine with no credential store is to go without,
    not to quietly write the password into a file."""
    ok, why = z.keyring_available()
    assert not ok
    assert "pip install keyring" in why
    with pytest.raises(RuntimeError, match="keyring"):
        z.save_credentials(USER, SECRET)
    assert z.stored_password(USER) == ""


def test_a_keyring_that_cannot_store_anything_is_not_offered(monkeypatch):
    """keyring installs a no-op backend when it finds no store. Writing to it
    silently succeeds and reads back nothing, which would look like the
    password being forgotten between runs."""
    module = types.ModuleType("keyring")
    module.get_keyring = lambda: type("fail", (), {})()
    monkeypatch.setitem(sys.modules, "keyring", module)
    ok, why = z.keyring_available()
    assert not ok
    assert "no credential store" in why
