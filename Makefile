.PHONY: all clean

all: src/Graphics/Vulkan/Bindings.hs
	stack build

src/Graphics/Vulkan/Bindings.hs: apigen/*.hs
	(cd apigen; stack build :vulkan-apigen && \
	 stack exec vulkan-apigen vk.xml ../src/Graphics/Vulkan )

clean:
	rm -r src/Graphics/Vulkan/Bindings.hs
