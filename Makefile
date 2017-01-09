.PHONY: all clean

VK_SPEC := apigen/Vulkan-Docs/src/spec/vk.xml
MAN_DIR := apigen/Vulkan-Docs/doc/specs/vulkan/man
OUT_DIR := src/Graphics/Vulkan

all: $(OUT_DIR)/Bindings.hs
	stack build --haddock

src/Graphics/Vulkan/Bindings.hs: apigen/*.hs $(VK_SPEC)
	(cd apigen; stack build :vulkan-apigen && \
	 stack exec vulkan-apigen ../$(VK_SPEC) ../$(OUT_DIR) \
	   ../$(MAN_DIR)/{V,v}k*.txt )

clean:
	rm -r $(OUT_DIR)/Bindings.hs
