use lam_emu::Ref;
use log::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use web_sys::*;

#[derive(PartialEq, Debug, Clone)]
#[repr(C)]
pub struct WebRef {
    pub ref_: Ref,
}

thread_local! {
    pub static WEB_SYS__ELEMENTS: RefCell<HashMap<u128, Rc<RefCell<Element>>>> = RefCell::new(HashMap::new());
}
pub const WEB_SYS__ELEMENT: u128 = 0;

impl Into<Ref> for WebRef {
    fn into(self) -> Ref {
        self.ref_
    }
}

impl Into<WebRef> for Element {
    fn into(self) -> WebRef {
        WEB_SYS__ELEMENTS.with(|kv| {
            let tag = WEB_SYS__ELEMENT;
            let id = (kv.borrow().len() as u128) + 1;
            kv.borrow_mut()
                .insert(id, Rc::new(RefCell::new(self.clone())));
            WebRef {
                ref_: Ref { tag, id },
            }
        })
    }
}

impl From<WebRef> for Rc<RefCell<Element>> {
    fn from(webref: WebRef) -> Rc<RefCell<Element>> {
        debug!("Turning {:?} into element", webref);
        if webref.ref_.tag != WEB_SYS__ELEMENT {
            panic!(
                "Wrong reference tag: {:?}, expected {:?}",
                webref.ref_.tag, WEB_SYS__ELEMENT
            );
        }
        WEB_SYS__ELEMENTS.with(|kv| kv.borrow().get(&webref.ref_.id).unwrap().clone())
    }
}
