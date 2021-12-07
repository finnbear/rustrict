#[cfg(feature = "customize")]
use std::cell::UnsafeCell;
use std::ops::Deref;

/// Allows (unsafe) mutation if the "customize" feature is enabled. In this case, mutation must
/// not be concurrent. Otherwise, 100% safe.
pub(crate) struct FeatureCell<T> {
    #[cfg(feature = "customize")]
    inner: UnsafeCell<T>,
    #[cfg(not(feature = "customize"))]
    inner: T,
}

impl<T> FeatureCell<T> {
    pub fn new(val: T) -> Self {
        Self {
            #[cfg(feature = "customize")]
            inner: UnsafeCell::new(val),
            #[cfg(not(feature = "customize"))]
            inner: val,
        }
    }

    /// SAFETY: Caller must avoid concurrent access, in accordance with documentation.
    #[cfg(feature = "customize")]
    pub unsafe fn get_mut(&self) -> &mut T {
        &mut *self.inner.get()
    }
}

impl<T> Deref for FeatureCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        #[cfg(not(feature = "customize"))]
        return &self.inner;
        // SAFETY: User must avoid concurrent access, in accordance with documentation.
        #[cfg(feature = "customize")]
        unsafe {
            &*self.inner.get()
        }
    }
}

// SAFETY: User must avoid concurrent access, in accordance with documentation.
#[cfg(feature = "customize")]
unsafe impl<T> Send for FeatureCell<T> {}
unsafe impl<T> Sync for FeatureCell<T> {}
