function toggle (attr) {
  attr = 'toggle-' + attr;
  if (document.body.hasAttribute(attr)) {
    document.body.removeAttribute(attr);
  } else {
    document.body.setAttribute(attr, '');
  }
}
